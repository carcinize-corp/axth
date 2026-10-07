package dev.axllm.ax;

import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * The host side of the MCP Apps protocol: one sandboxed frame joined to one
 * MCP client.
 *
 * <p>Every policy decision belongs to Core, in {@code ir/axcore/mcp.axir}:
 * {@code mcp_app_tool_meta} and {@code mcp_app_tool_visible_to} for the ui
 * resource and visibility, {@code mcp_app_resource_plan} for the scheme,
 * MIME type and HTML document, and {@code mcp_app_view_message_plan} for the
 * whole dispatch including the initialization gate, reserved sandbox methods
 * and display-mode validation. What is native here is only effect: reading
 * the resource, decoding a blob, calling the host's callbacks, tracking the
 * initialized flag and the outbound id, and shaping the JSON-RPC envelope
 * Core named. There is no second copy of the CSP or visibility rules here.
 *
 * <p>Two safety properties are easy to lose and so are stated: a frame
 * cannot act before it has initialized, and an absent host callback is a
 * closed door rather than a default this bridge invents.
 */
public final class AxMCPAppBridge {
  /** The display modes the MCP Apps protocol defines. */
  private static final List<String> DISPLAY_MODES = List.of("inline", "fullscreen", "pip");

  private final AxMCPClient client;
  private final Map<String, Object> tool;
  private final Map<String, Object> options;
  private boolean initialized;
  private int nextId = 1;

  /**
   * A bridge for one tool.
   *
   * @param tool a tool name, or a tool object from the client's catalog
   * @param options host callbacks, all optional: {@code sendToView},
   *     {@code hostCapabilities}, {@code hostContext}, {@code authorize},
   *     {@code openLink}, {@code sendMessage}, {@code updateModelContext},
   *     {@code requestDisplayMode}, {@code log} and {@code sizeChanged}
   */
  public AxMCPAppBridge(AxMCPClient client, Object tool, Map<String, Object> options) {
    this.client = client;
    this.options = new LinkedHashMap<>(options == null ? Map.of() : options);
    if (tool instanceof Map<?, ?> provided) {
      this.tool = Core.asMap(provided);
    } else {
      String name = String.valueOf(tool);
      this.tool = client.getTools().stream()
          .filter(candidate -> name.equals(String.valueOf(candidate.get("name"))))
          .findFirst()
          .orElseThrow(() -> new AxMCPError("MCP App tool not found: " + name));
    }
  }

  public AxMCPAppBridge(AxMCPClient client, Object tool) {
    this(client, tool, Map.of());
  }

  public boolean isInitialized() {
    return initialized;
  }

  public Map<String, Object> getTool() {
    return Map.copyOf(tool);
  }

  /**
   * Reads, validates and returns this App's {@code ui://} resource.
   *
   * <p>A resource whose URI is not {@code ui://}, whose MIME type is not the
   * App type, whose body is not an HTML document, or whose CSP names an
   * unsafe source, is refused here rather than handed to a frame.
   */
  public Map<String, Object> loadResource() {
    String name = String.valueOf(tool.get("name"));
    String uri = String.valueOf(Core.asMap(Core.mcp_app_tool_meta(tool)).getOrDefault("resourceUri", ""));
    if (!uri.startsWith("ui://")) {
      throw new AxMCPError("MCP App tool " + name + " has no valid ui:// resource");
    }
    Map<String, Object> response = client.readResource(uri);
    Map<String, Object> item = Core.asList(response.get("contents")).stream()
        .map(Core::asMap)
        .filter(candidate -> uri.equals(String.valueOf(candidate.get("uri"))))
        .findFirst()
        .orElseThrow(() -> new AxMCPError("MCP App resource " + uri + " was not returned"));
    String html;
    if (item.get("text") instanceof String text) {
      html = text;
    } else {
      try {
        html = new String(Base64.getDecoder().decode(String.valueOf(item.getOrDefault("blob", ""))),
            java.nio.charset.StandardCharsets.UTF_8);
      } catch (IllegalArgumentException error) {
        throw new AxMCPError("MCP App resource blob is not valid base64 HTML");
      }
    }
    Object meta = Core.asMap(item.get("_meta")).get("ui");
    Map<String, Object> plan = Core.asMap(Core.mcp_app_resource_plan(name, uri,
        item.getOrDefault("mimeType", "<missing>"), html,
        meta instanceof Map<?, ?> ? Core.asMap(meta) : Map.of()));
    if (!Boolean.TRUE.equals(plan.get("ok"))) {
      throw new AxMCPError(String.valueOf(plan.get("message")));
    }
    return Core.asMap(plan.get("resource"));
  }

  /**
   * Dispatches one message from the frame.
   *
   * @return the response to send back, or null for a notification
   */
  public Map<String, Object> handleViewMessage(Map<String, Object> message) {
    try {
      return dispatch(message);
    } catch (RuntimeException error) {
      // A host callback that fails becomes a JSON-RPC error when the message
      // carried an id, and propagates when it did not, so a notification
      // failure is never swallowed.
      if (!message.containsKey("id")) {
        throw error;
      }
      // Map.of refuses a null value, and a JSON-RPC id may legitimately be
      // null, so the envelope is built with a null-capable map.
      Map<String, Object> failure = new LinkedHashMap<>();
      failure.put("code", -32000);
      failure.put("message", String.valueOf(error.getMessage()));
      Map<String, Object> envelope = new LinkedHashMap<>();
      envelope.put("jsonrpc", "2.0");
      envelope.put("id", message.get("id"));
      envelope.put("error", failure);
      return envelope;
    }
  }

  private Map<String, Object> dispatch(Map<String, Object> message) {
    Map<String, Object> context = new LinkedHashMap<>();
    context.put("namespace", client.namespace());
    context.put("tool", tool.get("name"));
    context.put("tools", new ArrayList<Object>(client.getTools()));
    context.put("hostCapabilities", options.get("hostCapabilities"));
    context.put("hostContext", options.get("hostContext"));
    context.put("canOpenLink", consumer("openLink") != null);
    context.put("canSendMessage", consumer("sendMessage") != null);
    context.put("canUpdateModelContext", consumer("updateModelContext") != null);
    Map<String, Object> plan = Core.asMap(Core.mcp_app_view_message_plan(message, initialized, context));
    String action = String.valueOf(plan.get("action"));
    switch (action) {
      case "error":
        throw new AxMCPError(String.valueOf(plan.get("reason")));
      case "initialized":
        initialized = true;
        return null;
      case "ignore":
        return null;
      case "log": {
        Consumer<Object> callback = consumer("log");
        if (callback != null) {
          callback.accept(plan.get("params"));
        }
        return null;
      }
      case "size-changed": {
        Consumer<Object> callback = consumer("sizeChanged");
        if (callback != null) {
          callback.accept(plan.get("size"));
        }
        return null;
      }
      default:
        break;
    }
    Object result = Map.of();
    if ("respond".equals(action)) {
      result = plan.get("result");
    } else {
      authorize(action, message);
      switch (action) {
        case "call-tool":
          result = client.callTool(String.valueOf(plan.get("name")), Core.asMap(plan.get("arguments")));
          break;
        case "read-resource":
          result = client.readResource(String.valueOf(plan.get("uri")));
          break;
        case "open-link":
          consumer("openLink").accept(plan.get("url"));
          break;
        case "send-message":
          consumer("sendMessage").accept(plan.get("params"));
          break;
        case "update-model-context":
          // Core stamped untrusted and the source; pass it through unchanged.
          consumer("updateModelContext").accept(plan.get("update"));
          break;
        case "request-display-mode": {
          Object callback = options.get("requestDisplayMode");
          Object mode = "inline";
          if (callback instanceof Function<?, ?>) {
            @SuppressWarnings("unchecked")
            Function<Object, Object> granted = (Function<Object, Object>) callback;
            mode = granted.apply(plan.get("mode"));
          }
          // Core validated the mode the frame asked for; this validates the
          // mode the host granted. A host that answers with a mode the
          // protocol does not define is a host bug, and the frame must not
          // be told it succeeded.
          if (!DISPLAY_MODES.contains(String.valueOf(mode))) {
            throw new AxMCPError("Invalid MCP App display mode granted by host: " + mode);
          }
          result = new LinkedHashMap<>(Map.of("mode", mode));
          break;
        }
        default:
          throw new AxMCPError("Unknown MCP App action: " + action);
      }
    }
    Map<String, Object> response = new LinkedHashMap<>();
    response.put("jsonrpc", "2.0");
    response.put("id", message.get("id"));
    response.put("result", result == null ? Map.of() : result);
    return response;
  }

  private void authorize(String action, Map<String, Object> message) {
    Object callback = options.get("authorize");
    if (!(callback instanceof Function<?, ?>)) {
      return;
    }
    @SuppressWarnings("unchecked")
    Function<Object, Object> decide = (Function<Object, Object>) callback;
    Map<String, Object> request = new LinkedHashMap<>();
    request.put("method", action);
    request.put("params", message.get("params"));
    request.put("namespace", client.namespace());
    request.put("tool", tool.get("name"));
    if (Boolean.FALSE.equals(decide.apply(request))) {
      throw new AxMCPError("MCP App request denied: " + action);
    }
  }

  @SuppressWarnings("unchecked")
  private Consumer<Object> consumer(String name) {
    Object value = options.get(name);
    return value instanceof Consumer<?> ? (Consumer<Object>) value : null;
  }

  private void notify(String method, Object params) {
    if (!initialized) {
      throw new AxMCPError("MCP App is not initialized");
    }
    Consumer<Object> callback = consumer("sendToView");
    if (callback != null) {
      Map<String, Object> message = new LinkedHashMap<>();
      message.put("jsonrpc", "2.0");
      message.put("method", method);
      message.put("params", params);
      callback.accept(message);
    }
  }

  public void notifyToolInput(Object arguments) {
    notify("ui/notifications/tool-input", singleEntry("arguments", arguments));
  }

  public void notifyToolInputPartial(Object arguments) {
    notify("ui/notifications/tool-input-partial", singleEntry("arguments", arguments));
  }

  public void notifyToolResult(Object result) {
    notify("ui/notifications/tool-result", result);
  }

  public void notifyToolCancelled(Object reason) {
    notify("ui/notifications/tool-cancelled", singleEntry("reason", reason));
  }

  public void notifyHostContextChanged(Object context) {
    notify("ui/notifications/host-context-changed", context);
  }

  /**
   * Tears the frame down and requires a fresh initialization afterwards, so
   * a torn-down App cannot keep pushing notifications.
   */
  public void teardown(Object reason) {
    int id = nextId++;
    Consumer<Object> callback = consumer("sendToView");
    if (callback != null) {
      Map<String, Object> message = new LinkedHashMap<>();
      message.put("jsonrpc", "2.0");
      message.put("id", id);
      message.put("method", "ui/resource-teardown");
      message.put("params", singleEntry("reason", reason));
      callback.accept(message);
    }
    initialized = false;
  }

  /** A one-entry map that accepts a null value, unlike Map.of. */
  private static Map<String, Object> singleEntry(String key, Object value) {
    Map<String, Object> out = new LinkedHashMap<>();
    out.put(key, value);
    return out;
  }

  /** TOOL's App metadata: resourceUri, visibility, hasVisibility. */
  public static Map<String, Object> toolMeta(Map<String, Object> tool) {
    return Core.asMap(Core.mcp_app_tool_meta(tool));
  }

  /** Whether PRINCIPAL, "model" or "app", may call TOOL. */
  public static boolean toolVisibleTo(Map<String, Object> tool, String principal) {
    return Boolean.TRUE.equals(Core.mcp_app_tool_visible_to(tool, principal));
  }

  static List<Object> listOf(Object value) {
    return Core.asList(value);
  }
}
