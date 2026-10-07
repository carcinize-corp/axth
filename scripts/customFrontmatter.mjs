// @ts-check
import { MarkdownPageEvent } from 'typedoc-plugin-markdown';

/**
 * @param {import('typedoc-plugin-markdown').MarkdownApplication} app
 */
export function load(app) {
  app.renderer.on(MarkdownPageEvent.BEGIN, (page) => {
    // Update frontmatter with the page title
    page.frontmatter = {
      title: page.model?.name,
      ...page.frontmatter,
    };
  });

  app.renderer.on(MarkdownPageEvent.END, (page) => {
    // Transform specific link patterns in the page content
    page.contents = replaceAndFormat(page.contents);
  });
}

/**
 * Transforms markdown link paths to a specific format.
 * Examples:
 * [`AxChatResponse`](TypeAlias.AxChatResponse.md) -> [`AxChatResponse`](/apidocs/typealiasaxchatresponse/)
 *
 * @param {string | undefined} input - The input markdown content
 * @returns {string | undefined} Transformed markdown content
 */
function replaceAndFormat(input) {
  if (!input) return input;

  return input.replace(
    /(\[`?[^`\]]+`?\]\()([^)]+)(\))/g,
    (match, linkText, path, closing) => {
      // Only local TypeDoc pages need rewriting. Preserve external links,
      // same-page anchors, and the fragment's spelling (including underscores).
      const local = path.match(/^(?:\.\/)?([^/#:]+)\.md(#[^\s]*)?$/);
      if (!local) return match;
      const slug = local[1].toLowerCase().replace(/[^a-z0-9-]/g, '');
      return `${linkText}/apidocs/${slug}/${local[2] ?? ''}${closing}`;
    }
  );
}
