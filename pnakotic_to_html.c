// =============================================================================
// Some notes.
//
// We use int instead of size_t for everything, so we have some issue
// with conversion warnings here but whatever.
//
// The output html buffer is not sanitized properly. In particular the link node
// allows stuff like <<javascript:alert('asdf')>>.
// =============================================================================
#include <stdio.h>
#include <stdlib.h>

#define PNAKOTIC_IMPLEMENTATION
#include "pnakotic.h"


// =============================================================================
// @@@ Emitter / Data
// =============================================================================
typedef struct String_Fragment {
    char  *buf;
    int buf_payload;
    struct String_Fragment *next;
} String_Fragment;

typedef struct {
    Pn_Parser parser;
    String_Fragment *first_fragment, *last_fragment;
    int fragment_capacity;

    char  num_buf[16]; // For serializing a number. Done by parse_num()
    char *num_cursor;
    int   num_len;
} Emitter;


// =============================================================================
// @@@ Emitter / Functions
// =============================================================================
static char *finalize (Emitter *emitter) {
    int buf_len = 0;
    String_Fragment *frag = emitter->first_fragment;
    while (frag) {
        buf_len += frag->buf_payload;
        frag = frag->next;
    }

    char *buf = malloc(buf_len + 1);
    char *buf_cursor = buf;
    frag = emitter->first_fragment;
    while (frag) {
        if (buf) {
            memcpy(buf_cursor, frag->buf, frag->buf_payload);
            buf_cursor += frag->buf_payload;
        }
        String_Fragment *tmp = frag;
        frag = frag->next;
        free(tmp->buf);
        free(tmp);
    }

    pn_free(emitter->parser);
    if (buf) buf[buf_len] = '\0';
    return buf;
}

static void make_fragment (Emitter *emitter) {
    String_Fragment *tmp = calloc(1, sizeof(*tmp));
    tmp->buf                     = calloc(1, emitter->fragment_capacity);
    emitter->last_fragment->next = tmp;
    emitter->last_fragment       = tmp;
}

static void write_to_fragment (Emitter *emitter, char *str, int str_len) {
    char *top = emitter->last_fragment->buf + emitter->last_fragment->buf_payload;
    memcpy(top, str, str_len);
    emitter->last_fragment->buf_payload += str_len;
}

static void append_n (Emitter *emitter, char *str, int str_len) {
    if (str_len == 0) return;

    while (1) {
        int available = emitter->fragment_capacity - emitter->last_fragment->buf_payload;

        if (str_len <= available) {
            write_to_fragment(emitter, str, str_len);
            break;
        } else {
            write_to_fragment(emitter, str, available);
            make_fragment(emitter);
            str     += available;
            str_len -= available;
        }
    }
}

static void append (Emitter *emitter, char *buf) {
    append_n(emitter, buf, strlen(buf));
}

// Just a minimal sanitization to allow typing stuff like <>.
static void append_tok (Emitter *emitter, Pn_Token *token) {
    switch (token->type) {
    case '<': for (int i = 0; i < token->txt.len; ++i) append(emitter, "&lt;"); break;
    case '>': for (int i = 0; i < token->txt.len; ++i) append(emitter, "&gt"); break;
    case '&': for (int i = 0; i < token->txt.len; ++i) append(emitter, "&amp;"); break;
    case '"': for (int i = 0; i < token->txt.len; ++i) append(emitter, "&quot;"); break;
    default:  append_n(emitter, token->txt.buf, token->txt.len);
    }
}

static void parse_num (Emitter *emitter, int num) {
    char *cursor = emitter->num_buf;
    char *end    = cursor + 9;

    while (cursor != end) {
        int tmp = num;
        *cursor++ = "0123456789"[tmp - (num /= 10) * 10];
    }

    char ch;
    char *left_cursor  = emitter->num_buf;
    char *right_cursor = cursor - 1;

    while (right_cursor > left_cursor) {
        ch            = *left_cursor;
        *left_cursor  = *right_cursor;
        *right_cursor = ch;
        left_cursor++;
        right_cursor--;
    }

    int n = 9;
    cursor = emitter->num_buf;
    while (*cursor == '0') { n--; cursor++; }

    emitter->num_len = n;
    emitter->num_cursor = cursor;
}

static void on_link_enter (Emitter *emitter) {
    int found_content = 0;
    append(emitter, "<a href=\"");

    while (1) {
        Pn_Event *e = pn_next(emitter->parser);
        switch (e->type) {
        default: break;

        case PN_EVENT_NODE_EXIT: {
            if (! found_content) {
                append(emitter, "\">");
                append_n(emitter, e->as.node->as.link.ref.buf, e->as.node->as.link.ref.len);
            }
            append(emitter, "</a>");
        } return;

        case PN_EVENT_TEXT_LINK_ALIAS: {
            if (! found_content) { found_content = 1; append(emitter, "\">"); }
            append_tok(emitter, e->as.token);
        } break;

        case PN_EVENT_TEXT_LINK_REF: append_tok(emitter, e->as.token); break;
        }
    }
}

static void on_table_cell_enter (Emitter *emitter, Pn_Ast *node) {
    append(emitter, "<td");

    if (node->as.table_cell.width > 1) {
        append(emitter, " colspan='");
        parse_num(emitter, node->as.table_cell.width);
        append_n(emitter, emitter->num_cursor, emitter->num_len);
        append(emitter, "'");
    }

    if (node->as.table_cell.height > 1) {
        append(emitter, " rowspan='");
        parse_num(emitter, node->as.table_cell.height);
        append_n(emitter, emitter->num_cursor, emitter->num_len);
        append(emitter, "'");
    }

    append(emitter, ">");
}

// A little wrapper around pn_next for processing meta text.
static Pn_Event *meta_next (Emitter *emitter) {
    Pn_Event *e = pn_next(emitter->parser);
    if (e->type != PN_EVENT_TEXT_META) return NULL;

    while (e->as.token->type == PN_TOKEN_WHITESPACE || e->as.token->type == PN_TOKEN_NEWLINE) {
        e = pn_next(emitter->parser);
        if (e->type != PN_EVENT_TEXT_META) return NULL;
    }

    return e;
}

static void parse_color (Emitter *emitter) {
    Pn_Event *e = meta_next(emitter);
    if (!e) return;

    if (e->as.token->type != '#') {
        append_tok(emitter, e->as.token);
    } else {
        e = pn_next(emitter->parser);
        if (e->type == PN_EVENT_TEXT_META) {
            append(emitter, "#");
            append_tok(emitter, e->as.token);
        } else {
            append(emitter, "#fff");
        }
    }
}

static int match (char *str, Pn_Token *token) {
    int str_len = strlen(str);
    if (token->txt.len != str_len) return 0;
    return !strncmp(str, token->txt.buf, token->txt.len);
}

static void parse_meta (Emitter *emitter, Pn_Ast *node, int is_block) {
    Pn_Event *e = meta_next(emitter);

    if (! e) {
        append(emitter, ">");
    } else if (match("fg", e->as.token)) {
        append(emitter, "style='color: ");
        parse_color(emitter);
        append(emitter, ";'>");
    } else if (match("bg", e->as.token)) {
        append(emitter, "style='padding: 0 .2rem; border-radius: 2px; background: ");
        parse_color(emitter);
        append(emitter, ";'>");
    } else if (is_block && match("mark", e->as.token)) {
        append(emitter, "style='border-left: 2px solid ");
        parse_color(emitter);
        append(emitter, "; background: rgba(255, 255, 255, .05);'>");
    } else if (is_block && match("NOTE", e->as.token)) {
        append(emitter, " class='note'><div class='title'>Note ");
        while ((e = pn_next(emitter->parser))->type == PN_EVENT_TEXT_META) append_tok(emitter, e->as.token);
        append(emitter, "</div>");
    } else if (is_block && match("TODO", e->as.token)) {
        append(emitter, " class='todo'><div class='title'>Todo ");
        while ((e = pn_next(emitter->parser))->type == PN_EVENT_TEXT_META) append_tok(emitter, e->as.token);
        append(emitter, "</div>");
    } else if (is_block && match("TIP", e->as.token)) {
        append(emitter, " class='tip'><div class='title'>Tip ");
        while ((e = pn_next(emitter->parser))->type == PN_EVENT_TEXT_META) append_tok(emitter, e->as.token);
        append(emitter, "</div>");
    } else if (is_block && match("WARNING", e->as.token)) {
        append(emitter, " class='warning'><div class='title'>Warning ");
        while ((e = pn_next(emitter->parser))->type == PN_EVENT_TEXT_META) append_tok(emitter, e->as.token);
        append(emitter, "</div>");
    } else if (is_block && match("sidenote", e->as.token)) {
        append(emitter, " class='aside'><div class='title'>");
        while ((e = pn_next(emitter->parser))->type == PN_EVENT_TEXT_META) append_tok(emitter, e->as.token);
        append(emitter, "</div>");
    } else if (match("spoiler", e->as.token)) {
        append(emitter, " class='spoiler'>");
        while ((e = pn_next(emitter->parser))->type == PN_EVENT_TEXT_META) append_tok(emitter, e->as.token);
    } else if (match("img", e->as.token)) {
        append(emitter, "><img src='");
        while ((e = pn_next(emitter->parser))->type == PN_EVENT_TEXT_META) append_tok(emitter, e->as.token);
        append(emitter, "'>");
    } else if (is_block && match("table_style_transparent", e->as.token)) {
        append(emitter, "class='table-style-transparent'>");
    } else if (is_block && match(">", e->as.token)) {
        node->type = -1; // Mark for on_node_exit().
        append(emitter, "><details><summary>");
        while ((e = pn_next(emitter->parser))->type == PN_EVENT_TEXT_META) append_tok(emitter, e->as.token);
        append(emitter, "</summary>");
    } else {
        append(emitter, ">");
    }
}

static void on_node_enter (Emitter *emitter, Pn_Ast *node) {
    switch (node->type) {
    default: break;
    case PN_AST_PARAGRAPH:           append(emitter, "<p>"); break;
    case PN_AST_PARAGRAPH_BLANK:     append(emitter, "<p class=\"blank-paragraph\">"); break;
    case PN_AST_LINK:                on_link_enter(emitter); break;
    case PN_AST_FORMAT_BLOCK:        append(emitter, "<pre class=\"format\">"); break;
    case PN_AST_FORMAT_BLOCK_STRICT: append(emitter, "<pre class=\"format-strict\">"); break;
    case PN_AST_FORMAT_INLINE:       append(emitter, "<span class=\"format-inline\">"); break;
    case PN_AST_BLOCKQUOTE:          append(emitter, "<blockquote>"); break;
    case PN_AST_STRIKETHROUGH:       append(emitter, "<s>"); break;
    case PN_AST_LINE_BREAK:          append(emitter, "<br>"); break;
    case PN_AST_SEPARATOR:           append(emitter, "<hr>"); break;
    case PN_AST_TABLE:               append(emitter, "<table width=\"100%\">"); break;
    case PN_AST_TABLE_CELL:          on_table_cell_enter(emitter, node); break;
    case PN_AST_SUB_SCRIPT:          append(emitter, "<sub>"); break;
    case PN_AST_SUP_SCRIPT:          append(emitter, "<sup>"); break;
    case PN_AST_LIST:                append(emitter, "<ul class=\"no-marker\">"); break;
    case PN_AST_LIST_ITEM:           append(emitter, "<li>"); break;
    case PN_AST_LIST_BY_BULLET:      append(emitter, "<ul>"); break;
    case PN_AST_LIST_BY_BULLET_ITEM: append(emitter, "<li>"); break;
    case PN_AST_LIST_BY_NUMBER:      append(emitter, "<ol>"); break;
    case PN_AST_LIST_BY_NUMBER_ITEM: append(emitter, "<li>"); break;
    case PN_AST_LIST_BY_CHECKBOX:    append(emitter, "<ul class=\"by-checkbox\">"); break;
    case PN_AST_META_INLINE:         append(emitter, "<span "); parse_meta(emitter, node, 0); break;
    case PN_AST_META_TREE:           append(emitter, "<div "); parse_meta(emitter, node, 1); break;
    case PN_AST_META_BLOCK:          append(emitter, "<div "); parse_meta(emitter, node, 1); break;

    case PN_AST_LIST_BY_CHECKBOX_ITEM: {
        append(emitter, "<li><input style=\"float: left; margin-left: -1.7em;\" type=\"checkbox\" disabled=\"\"");
        if (node->as.checked_list_item.is_checked) append(emitter, "checked=\"\"");
        append(emitter, ">");
    } break;

    case PN_AST_EMPHASIS: {
        switch (node->as.emphasis.lvl) {
        case 1:  append(emitter, "<i>"); break;
        case 2:  append(emitter, "<b>"); break;
        case 3:  append(emitter, "<b><i>"); break;
        default: append(emitter, "<span class=\"cthulhu-fhtagn\">"); break;
        }
    } break;

    case PN_AST_HEADER: {
        switch (node->as.header.lvl) {
        case 1:  append(emitter, "<h1>"); break;
        case 2:  append(emitter, "<h2>"); break;
        case 3:  append(emitter, "<h3>"); break;
        case 4:  append(emitter, "<h4>"); break;
        case 5:  append(emitter, "<h5>"); break;
        default: append(emitter, "<h6>"); break;
        }

        if (node->as.header.tag.len) {
            append(emitter, "<span class=\"header-tag\">");
            append_n(emitter, node->as.header.tag.buf, node->as.header.tag.len);
            append(emitter, "&nbsp;</span>");
        }
    } break;
    }
}

static void on_node_exit (Emitter *emitter, Pn_Ast *node) {
    switch (node->type) {
    default: break;
    case PN_AST_PARAGRAPH:             append(emitter, "</p>"); break;
    case PN_AST_PARAGRAPH_BLANK:       append(emitter, "</p>"); break;
    case PN_AST_FORMAT_BLOCK:          append(emitter, "</pre>"); break;
    case PN_AST_FORMAT_BLOCK_STRICT:   append(emitter, "</pre>"); break;
    case PN_AST_FORMAT_INLINE:         append(emitter, "</span>"); break;
    case PN_AST_STRIKETHROUGH:         append(emitter, "</s>"); break;
    case PN_AST_SUB_SCRIPT:            append(emitter, "</sub>"); break;
    case PN_AST_SUP_SCRIPT:            append(emitter, "</sup>"); break;
    case PN_AST_TABLE:                 append(emitter, "</tr>\n</table>\n"); break;
    case PN_AST_TABLE_CELL:            append(emitter, "</td>\n"); break;
    case PN_AST_LIST:                  append(emitter, "</ul>"); break;
    case PN_AST_LIST_ITEM:             append(emitter, "</li>"); break;
    case PN_AST_LIST_BY_BULLET:        append(emitter, "</ul>"); break;
    case PN_AST_LIST_BY_BULLET_ITEM:   append(emitter, "</li>"); break;
    case PN_AST_LIST_BY_NUMBER:        append(emitter, "</ol>"); break;
    case PN_AST_LIST_BY_NUMBER_ITEM:   append(emitter, "</li>"); break;
    case PN_AST_LIST_BY_CHECKBOX:      append(emitter, "</ul>"); break;
    case PN_AST_LIST_BY_CHECKBOX_ITEM: append(emitter, "</li>"); break;
    case PN_AST_BLOCKQUOTE:            append(emitter, "</blockquote>"); break;
    case PN_AST_META_INLINE:           append(emitter, "</span>"); break;

    case PN_AST_META_TREE:
    case PN_AST_META_BLOCK: {
        if (node->type == -1) append(emitter, "</details>");
        append(emitter, "</div>");
    } break;

    case PN_AST_EMPHASIS: {
        switch (node->as.emphasis.lvl) {
        case 1:  append(emitter, "</i>"); break;
        case 2:  append(emitter, "</b>"); break;
        case 3:  append(emitter, "</b></i>"); break;
        default: append(emitter, "</span>"); break;
        }
    } break;

    case PN_AST_HEADER: {
        switch (node->as.header.lvl) {
        case 1:  append(emitter, "</h1>"); break;
        case 2:  append(emitter, "</h2>"); break;
        case 3:  append(emitter, "</h3>"); break;
        case 4:  append(emitter, "</h4>"); break;
        case 5:  append(emitter, "</h5>"); break;
        default: append(emitter, "</h6>"); break;
        }
    } break;
    }
}

static void mainloop (Emitter *emitter) {
    while (1) {
        Pn_Event *e = pn_next(emitter->parser);
        switch (e->type) {
        case PN_EVENT_EOF:           return;
        case PN_EVENT_ALLOC_FAIL:    return;
        case PN_EVENT_NODE_ENTER:    on_node_enter(emitter, e->as.node); break;
        case PN_EVENT_NODE_EXIT:     on_node_exit(emitter, e->as.node); break;
        case PN_EVENT_TABLE_ROW_END: append(emitter, "</tr>\n\n<tr>\n"); break;
        case PN_EVENT_TEXT:          append_tok(emitter, e->as.token); break;
        default:                     break;
        }
    }
}

char *pnakotic_to_html (char *in, int in_len) {
    Emitter emitter             = {0};
    emitter.fragment_capacity   = in_len;
    emitter.first_fragment      = calloc(1, sizeof(String_Fragment));
    emitter.last_fragment       = emitter.first_fragment;
    emitter.first_fragment->buf = malloc(in_len);
    emitter.parser              = pn_new(in, in_len, 0);
    if (!emitter.first_fragment || !emitter.first_fragment->buf || !emitter.parser) goto ON_ERR;

    mainloop(&emitter);
    return finalize(&emitter);

ON_ERR:
    if (emitter.first_fragment) free(emitter.first_fragment->buf);
    free(emitter.first_fragment);
    if (emitter.parser) pn_free(emitter.parser);
    return NULL;
}

int main (void) {
    char *in   = "test";
    int in_len = strlen(in);
    char *out  = pnakotic_to_html(in, in_len);
    free(out);
    return EXIT_SUCCESS;
}
