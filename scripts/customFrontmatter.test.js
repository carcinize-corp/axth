import { MarkdownPageEvent } from 'typedoc-plugin-markdown';
import { describe, expect, it } from 'vitest';
import { load } from './customFrontmatter.mjs';

describe('TypeDoc website links', () => {
  const handlers = new Map();
  load({ renderer: { on: (event, handler) => handlers.set(event, handler) } });

  it('rewrites only the filename, preserving member anchors and link labels', () => {
    const page = {
      contents:
        '[`sequence_number`](Interface.StreamEvent.md#sequence_number)\n' +
        '[Event](./Interface.StreamEvent.md)\n' +
        '[member](#sequence_number)\n' +
        '[reference](https://example.com/api.md#member)\n' +
        '[local](http://example.com/api.md#member)',
    };
    handlers.get(MarkdownPageEvent.END)(page);
    expect(page.contents).toBe(
      '[`sequence_number`](/apidocs/interfacestreamevent/#sequence_number)\n' +
        '[Event](/apidocs/interfacestreamevent/)\n' +
        '[member](#sequence_number)\n' +
        '[reference](https://example.com/api.md#member)\n' +
        '[local](http://example.com/api.md#member)'
    );
  });
});
