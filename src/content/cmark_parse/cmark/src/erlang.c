#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "cmark_ctype.h"
#include "config.h"
#include "cmark.h"
#include "node.h"
#include "buffer.h"
#include "houdini.h"
#include "scanners.h"

#define BUFFER_SIZE 100

// Functions to convert cmark_nodes to ERLANG terms strings.

static void escape_html(cmark_strbuf *dest, const unsigned char *source,
                        bufsize_t length) {
cmark_strbuf_putc(dest, '"');
  //houdini_escape_html0(dest, source, length, 0);
cmark_strbuf_put(dest, source, length);
cmark_strbuf_putc(dest, '"');
}

static void escape_html_without_quote(cmark_strbuf *dest, const unsigned char *source,
                        bufsize_t length) {
  //houdini_escape_html0(dest, source, length, 0);
cmark_strbuf_put(dest, source, length);
}

static CMARK_INLINE void cr(cmark_strbuf *html) {
  if (html->size && (html->ptr[html->size - 1] != '\n' || html->ptr[html->size - 1] == ','))
    cmark_strbuf_putc(html, '\n');
}

struct render_state {
  cmark_strbuf *html;
  cmark_node *plain;
};

static int S_render_node(cmark_node *node, cmark_event_type ev_type,
                         struct render_state *state, int options) {
  cmark_node *parent;
  cmark_node *grandparent;
  cmark_strbuf *html = state->html;
  char start_heading[] = "{h0,[";
  bool tight;
  char buffer[BUFFER_SIZE];

  bool has_prev = node->prev != NULL;

  bool entering = (ev_type == CMARK_EVENT_ENTER);

  if (state->plain == node) { // back at original node
    state->plain = NULL;
  }

  if (state->plain != NULL) {
    switch (node->type) {
    case CMARK_NODE_TEXT:
    case CMARK_NODE_CODE:
    case CMARK_NODE_HTML_INLINE:
      escape_html_without_quote(html, node->as.literal.data, node->as.literal.len);
      break;

    case CMARK_NODE_LINEBREAK:
    case CMARK_NODE_SOFTBREAK:
      cmark_strbuf_putc(html, ' ');
      break;

    default:
      break;
    }
    return 1;
  }

  if (has_prev && entering) {
     cmark_strbuf_putc(html, ',');
  } 

  switch (node->type) {
  case CMARK_NODE_DOCUMENT:
    break;

  case CMARK_NODE_BLOCK_QUOTE:
    if (entering) {
      cr(html);
      cmark_strbuf_puts(html, "{blockquote,[");
    } else {
      cr(html);
      cmark_strbuf_puts(html, "]}");
    }
    break;

  case CMARK_NODE_LIST: {
    cmark_list_type list_type = node->as.list.list_type;
    int start = node->as.list.start;

    if (entering) {
      cr(html);
      if (list_type == CMARK_BULLET_LIST) {
        cmark_strbuf_puts(html, "{ul,[");
      } else if (start == 1) {
        cmark_strbuf_puts(html, "{{ol,1},[");
      } else {
        snprintf(buffer, BUFFER_SIZE, "{{ol,%d},[", start);
        cmark_strbuf_puts(html, buffer);
      }
    } else {
      cr(html);
      cmark_strbuf_puts(html, "]}");
    }
    break;
  }

  case CMARK_NODE_ITEM:
    if (entering) {
      cr(html);
      cmark_strbuf_puts(html, "{li,[");
    } else {
      cmark_strbuf_puts(html, "]}");
    }
    break;

  case CMARK_NODE_HEADING:
    if (entering) {
      cr(html);
      start_heading[2] = (char)('0' + node->as.heading.level);
      cmark_strbuf_puts(html, start_heading);
    } else {
      cmark_strbuf_puts(html, "]}");
    }
    break;

  case CMARK_NODE_CODE_BLOCK:
    cr(html);

    if (node->as.code.info.len == 0) {
      cmark_strbuf_puts(html, "{code,[");
    } else {
      bufsize_t first_tag = 0;
      while (first_tag < node->as.code.info.len &&
             !cmark_isspace(node->as.code.info.data[first_tag])) {
        first_tag += 1;
      }

      cmark_strbuf_puts(html, "{{code,\"language-");
      escape_html(html, node->as.code.info.data, first_tag);
      cmark_strbuf_puts(html, "\"},[");
    }

    escape_html(html, node->as.code.literal.data, node->as.code.literal.len);
    cmark_strbuf_puts(html, "]}");
    break;

  case CMARK_NODE_HTML_BLOCK:
    cr(html);
    if (options & CMARK_OPT_SAFE) {
      cmark_strbuf_puts(html, "<!-- raw HTML omitted -->");
    } else {
      cmark_strbuf_put(html, node->as.literal.data, node->as.literal.len);
    }
    cr(html);
    break;

  case CMARK_NODE_CUSTOM_BLOCK:
    cr(html);
    if (entering) {
      cmark_strbuf_put(html, node->as.custom.on_enter.data,
                       node->as.custom.on_enter.len);
    } else {
      cmark_strbuf_put(html, node->as.custom.on_exit.data,
                       node->as.custom.on_exit.len);
    }
    cr(html);
    break;

  case CMARK_NODE_THEMATIC_BREAK:
    cr(html);
    cmark_strbuf_puts(html, "{hr,[]}");
    break;

  case CMARK_NODE_PARAGRAPH:
    parent = cmark_node_parent(node);
    grandparent = cmark_node_parent(parent);
    if (grandparent != NULL && grandparent->type == CMARK_NODE_LIST) {
      tight = grandparent->as.list.tight;
    } else {
      tight = false;
    }
    if (!tight) {
      if (entering) {
        cr(html);
        cmark_strbuf_puts(html, "{p,[");
      } else {
        cmark_strbuf_puts(html, "]}");
      }
    }
    break;

  case CMARK_NODE_TEXT:
    escape_html(html, node->as.literal.data, node->as.literal.len);
    break;

  case CMARK_NODE_LINEBREAK:
    cmark_strbuf_puts(html, "{br,[]}");
    break;

  case CMARK_NODE_SOFTBREAK:
    if (options & CMARK_OPT_HARDBREAKS) {
      cmark_strbuf_puts(html, "{br,[]}\n");
    } else if (options & CMARK_OPT_NOBREAKS) {
      cmark_strbuf_putc(html, ' ');
    } else {
      //cmark_strbuf_putc(html, '\n');
	  cmark_strbuf_puts(html, "\"\\n\"");
    }
    break;

  case CMARK_NODE_CODE:
    cmark_strbuf_puts(html, "{code,[");
    escape_html(html, node->as.literal.data, node->as.literal.len);
    cmark_strbuf_puts(html, "]}");
    break;

  case CMARK_NODE_HTML_INLINE:
    if (options & CMARK_OPT_SAFE) {
      cmark_strbuf_puts(html, "<!-- raw HTML omitted -->");
    } else {
      cmark_strbuf_put(html, node->as.literal.data, node->as.literal.len);
    }
    break;

  case CMARK_NODE_CUSTOM_INLINE:
    if (entering) {
      cmark_strbuf_put(html, node->as.custom.on_enter.data,
                       node->as.custom.on_enter.len);
    } else {
      cmark_strbuf_put(html, node->as.custom.on_exit.data,
                       node->as.custom.on_exit.len);
    }
    break;

  case CMARK_NODE_STRONG:
    if (entering) {
      cmark_strbuf_puts(html, "{strong,[");
    } else {
      cmark_strbuf_puts(html, "]}");
    }
    break;

  case CMARK_NODE_EMPH:
    if (entering) {
      cmark_strbuf_puts(html, "{em,[");
    } else {
      cmark_strbuf_puts(html, "]}");
    }
    break;

  case CMARK_NODE_LINK:
    if (entering) {
      cr(html);
      cmark_strbuf_puts(html, "{a,{href,\"");
      if (!((options & CMARK_OPT_SAFE) &&
            scan_dangerous_url(&node->as.link.url, 0))) {
        houdini_escape_href(html, node->as.link.url.data,
                            node->as.link.url.len);
      }
      if (node->as.link.title.len) {
        cmark_strbuf_puts(html, "\"},{title,\"");
        escape_html(html, node->as.link.title.data, node->as.link.title.len);
        cmark_strbuf_puts(html, "\"},\"");
      }
      else {
		cmark_strbuf_puts(html, "\"},{title,undefined},\"");
	  }
	  state->plain = node;
    } else {
      cmark_strbuf_puts(html, "\"}");
    }
    break;

  case CMARK_NODE_IMAGE:
    if (entering) {
      cmark_strbuf_puts(html, "{img,{src,\"");
      if (!((options & CMARK_OPT_SAFE) &&
            scan_dangerous_url(&node->as.link.url, 0))) {
        houdini_escape_href(html, node->as.link.url.data,
                            node->as.link.url.len);
      }
      cmark_strbuf_puts(html, "\"},{alt,\"");
      state->plain = node;
    } else {
      if (node->as.link.title.len) {
        cmark_strbuf_puts(html, "\"},{title,\"");
        escape_html(html, node->as.link.title.data, node->as.link.title.len);
        cmark_strbuf_puts(html, "\"},");
      }
      else {
		cmark_strbuf_puts(html, "\"},{title,undefined}");
	  }
      cmark_strbuf_puts(html, "}");
    }
    break;

  default:
    assert(false);
    break;
  }

  // cmark_strbuf_putc(html, 'x');
  return 1;
}

char *cmark_render_erlang(cmark_node *root, int options) {
  char *result;
  cmark_strbuf html = CMARK_BUF_INIT(cmark_node_mem(root));
  cmark_event_type ev_type;
  cmark_node *cur;
  struct render_state state = {&html, NULL};
  cmark_iter *iter = cmark_iter_new(root);

  cmark_strbuf_puts(&html, "[\n");

  while ((ev_type = cmark_iter_next(iter)) != CMARK_EVENT_DONE) {
    cur = cmark_iter_get_node(iter);
    S_render_node(cur, ev_type, &state, options);
  }

  cmark_strbuf_puts(&html, "\n].");

  result = (char *)cmark_strbuf_detach(&html);

  cmark_iter_free(iter);
  return result;
}
