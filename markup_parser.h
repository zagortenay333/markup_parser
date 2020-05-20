// =============================================================================
//
// In order to include this library, in *only one* of your files do:
//     #define MARKUP_IMPLEMENTATION
//     #include "markup_parser.h"
//
// In all other files just do an #include to get the header.
//
// =============================================================================
//
// This parser can either create the full AST or just build one node at a time.
//
// Example (without making an AST):
//     M_Parser *pn = m_new(input_buf, input_buf_len, make_ast=0);
//
//     while (1) {
//         M_Event *e = m_next(pn);
//     }
//
//     m_free(pn);
//
//
// Example (making an AST):
//     M_Parser *pn = m_new(input_buf, input_buf_len, make_ast=1);
//     M_Ast *root  = m_get_root(pn);
//
//     // calls to m_next will keep expanding the tree now.
//
//     m_free(pn); // this also frees the tree.
//
//
// About the input_buf:
//     - It must be UTF-8 encoded.
//     - It doesn't need to be NUL-terminated.
//     - Lines can end with a single '\n' char or the sequence '\r\n'.
//     - Last line doesn't need to terminated with a newline char.
//
// =============================================================================
//
// On memory handling:
//
// When not building an AST:
//   - M_Ast nodes exist until you receive the M_EVENT_NODE_EXIT.
//   - Strings will be pointers into the original input_buf.
//   - Number of heap allocs is constant. The exception is when there
//     is a table that uses cell merging.
//
// When building an AST:
//   - Nodes are heap allocated. (Pools are used to minimize this.)
//   - Strings are pointers into an internal buffer (text copied from input_buf).
//   - After you've built the AST you can free the input_buf.
//
// In any case:
//   - You can supply custom allocators with m_new_with_alloc().
//   - You only have to do m_free().
//   - M_Event and M_Token exist until the next call to m_next().
//   - If a table uses cell merging, then it will temporarily heap allocate an
//     array of length equal to the number of columns. We're being lazy and
//     hardcoding the max number of columns...
//
// =============================================================================


#ifndef MARKUP_HEADER
#define MARKUP_HEADER

// =============================================================================
// @@@ Header
// =============================================================================
typedef struct {
    void *(*malloc) (size_t); // These should behave like the regular C funcs.
    void *(*calloc) (size_t, size_t);
    void  (*free)   (void *);
} M_Allocators;

typedef struct {
    char *buf; // Not NUL-terminated!
    int   len;
} M_String;

typedef int M_Token_Type;
enum {
    // Use ascii chars for token types directly.

    M_TOKEN_WORD = 128, // Sequence of a-zA-Z or any multibyte unicode char.
    M_TOKEN_NUMBER,     // Sequence of digits 0-9, but only if not next to a word. "1a" or "a1" are words.
    M_TOKEN_WHITESPACE, // Space or tab.
    M_TOKEN_NEWLINE,    // The single char '\n' or 2 char sequence "\r\n".
    M_TOKEN_EOF         // Doesn't correspond to a char. Means end of file.
};

typedef struct {
    M_String txt;
    M_Token_Type type;
    int line, column;
} M_Token;

typedef int M_Ast_Type;
enum {
    M_AST_ROOT,
    M_AST_PARAGRAPH,
    M_AST_PARAGRAPH_BLANK,
    M_AST_TABLE,
    M_AST_TABLE_CELL,
    M_AST_LIST,
    M_AST_LIST_ITEM,
    M_AST_LIST_BY_BULLET,
    M_AST_LIST_BY_BULLET_ITEM,
    M_AST_LIST_BY_NUMBER,
    M_AST_LIST_BY_NUMBER_ITEM,
    M_AST_LIST_BY_CHECKBOX,
    M_AST_LIST_BY_CHECKBOX_ITEM,
    M_AST_FORMAT_BLOCK,
    M_AST_FORMAT_BLOCK_STRICT,
    M_AST_FORMAT_INLINE,
    M_AST_META_TREE,
    M_AST_META_BLOCK,
    M_AST_META_INLINE,
    M_AST_HEADER,
    M_AST_COMMENT,
    M_AST_SEPARATOR,
    M_AST_BLOCKQUOTE,
    M_AST_LINK,
    M_AST_EMPHASIS,
    M_AST_LINE_BREAK,
    M_AST_SUP_SCRIPT,
    M_AST_SUB_SCRIPT,
    M_AST_STRIKETHROUGH
};

// Since we only do 1 pass on the input, a node will not have all the info
// available when you get to the M_EVENT_NODE_ENTER event.
//
// For example:
//   - start_line is available at node enter, and end_line at node_exit.
//
//   - link.ref is available right after you parsed it (when you get the
//     first M_EVENT_TEXT_LINK_ALIAS or a M_EVENT_NODE_EXIT).
//
// The txt prop is a little tricky:
//   - If you're not building an AST, then don't use it, just rely on the
//     stream of tokens.
//
//   - If you're dealing with blocks that contain text directly (like
//     paragraphs, or headers) then this is a pointer to an internal buffer
//     containing that text.  All delimiters and indentation are removed.
//     For example the txt of the paragaph inside this bullet node:
//         "- Here is the *paragraph*."
//     becomes:
//         "Here is the paragraph."
//
//   - If it's a link node, then this is the alias. That is, in the link
//     "<<www.whatever.com:: Whatever>> it's the text "Whatever".
//
//   - For nodes that don't directly contain text like list nodes, table cells,
//     etc..., this prop is largely useless, it's the combined text of all nodes
//     inside of it.
typedef struct M_Ast {
    M_Ast_Type type;

    int start_line, end_line;

    M_String txt; // Don't use if not making an AST.
    struct M_Ast *first_child, *right_sibling; // Don't use if not making an AST.

    union {
        struct { M_String ref; }                link;              // Available after parsing ref.
        struct { M_String data; }               meta;              // After parsing metadata.
        struct { M_String info; }               format_block;      // After parsing info string.
        struct { M_String tag; int lvl; }       header;            // At enter.
        struct { int lvl; }                     emphasis;          // At enter.
        struct { int is_checked; }              checked_list_item; // At enter.
        struct { int n_rows, n_cols; }          table;             // At exit.
        struct { int row, col, width, height; } table_cell;        // row/col at enter, width/height at exit.
    } as;
} M_Ast;

// An event either has a node/token payload, or no payload.
//
// Most ascii chars will always be returned as standalone tokens, and a sequence
// of the same char is always combined into 1 token (with exception of
// backslashes '\') For example: (;) (--) (!!!) (**), etc...
//
// The newline chars are always returned as 1 token M_TOKEN_NEWLINE.
//
// When it comes to letter, digits, spaces/tabs, and unicode multibyte chars;
// depending on the context they will either be returned as standalone tokens or
// merged into 1 token (a phrase):
//   - When you're inside a format node or parsing the data of a meta node (or
//     others; see bellow), then the above chars are always returned as standalone
//     tokens (M_TOKEN_WORD, M_TOKEN_NUMBER, M_TOKEN_WHITESPACE, M_TOKEN_NEWLINE)
//     so that this library can be more easily used as a lexer.
//
//   - When parsing regular text (like paragraphs, headers, etc..) the above will be
//     merged into 1 token when possible. That is, do not rely on the type of the
//     token here unless it's an ascii punctuation.
typedef enum {
    M_EVENT_NODE_ENTER,      // Node.
    M_EVENT_NODE_EXIT,       // Node.
    M_EVENT_TEXT,            // Token. returns phrases if not inside a format node.
    M_EVENT_TEXT_COMMENT,    // Token. returns phrases.
    M_EVENT_TEXT_LINK_REF,   // Token. returns proper tokens. <<link_ref:: link_alias>>.
    M_EVENT_TEXT_LINK_ALIAS, // Token. returns proper tokens.
    M_EVENT_TEXT_META,       // Token. returns proper tokens.
    M_EVENT_TEXT_META_DONE,  // No payload. Means no more meatadata text.
    M_EVENT_TABLE_ROW_END,   // No payload.
    M_EVENT_EOF,             // No payload.
    M_EVENT_ALLOC_FAIL,      // No payload.
    M_EVENT_NONE             // Never emitted; used internally.
} M_Event_Type;

typedef struct {
    int indentation;
    M_Event_Type type;
    union { M_Ast *node; M_Token *token; } as;
} M_Event;

typedef struct M_Parser *M_Parser;

M_Parser  m_new_with_alloc (char *buf, int buf_len, int make_ast, M_Allocators);
M_Parser  m_new            (char *buf, int buf_len, int make_ast);
void      m_free           (M_Parser);
M_Ast    *m_get_root       (M_Parser);
M_Event  *m_next           (M_Parser);


#endif // MARKUP_HEADER


// =============================================================================
// @@@ Implementation
//
// This is a recursive descent parser with an explicit stack.
//
// The parser acts like a finite-state machine, where the current state is
// in the top frame of the stack. That is, we have a stack of states.
//
// The stack holds frames of Parselets which are the individual parse funcs
// of the recursive descent parsers.
//
// For the most part we get away with a single pass on the input buffer.
// The exception is a table cell which specifies a width or height. In that
// case we must loop forward to count columns and/or rows.
// =============================================================================
#ifdef MARKUP_IMPLEMENTATION

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>


// =============================================================================
// @@@ Implementation / Lexer / Data
// =============================================================================
#define M_MAX_M_TOKEN_LOOKAHEAD 4 // Keep this a power of 2.

typedef struct {
    char *start, *end, *cursor;
    int  line, column, indentation;

    char last_line_has_newline;
    char at_start_of_line; // Doesn't mean column=1; it means we didn't eat any tokens after the indentation.

    M_Token token_ring[M_MAX_M_TOKEN_LOOKAHEAD];
    int      token_ring_cursor;
    int      token_ring_item_count;
} M_Lexer;


// =============================================================================
// @@@ Implementation / Lexer / Functions / Private
// =============================================================================
#define m_is_digit(C) (C >= '0' && C <= '9')
#define m_is_alpha(C) ((C >= 'a' && C <= 'z') || C == '_' || (unsigned char)C > 127 || (C >= 'A' && C <= 'Z'))

static int m_advance (M_Lexer *lex) {
    if (lex->cursor == lex->end) return 0;
    lex->column++;
    if (*lex->cursor++ == '\n') { lex->line++; lex->column = 1; }
    return 1;
}

static void m_make_token (M_Lexer *lex) {
    int index       = (lex->token_ring_cursor + lex->token_ring_item_count) & (M_MAX_M_TOKEN_LOOKAHEAD - 1);
    M_Token *token = &lex->token_ring[index];
    token->txt.buf  = lex->cursor;
    token->txt.len  = 1;
    token->type     = *token->txt.buf;
    token->line     = lex->line;
    token->column   = lex->column;

    lex->token_ring_item_count++;
    if (lex->cursor == lex->end) { token->type = M_TOKEN_EOF; return; }
    m_advance(lex);

    switch (*token->txt.buf) {
    case '\\': break; // Keep the backslash a 1 char token for escaping.
    case '\n': token->type = M_TOKEN_NEWLINE; break;
    case '\r': if (*lex->cursor == '\n') { token->type = M_TOKEN_NEWLINE; token->txt.len++; m_advance(lex); } break;

    case ' ': case '\t': {
        token->type = M_TOKEN_WHITESPACE;
        char *start = lex->cursor;
        while ((*lex->cursor == ' ' || *lex->cursor == '\t') && m_advance(lex));
        token->txt.len += (int)(lex->cursor - start);
   } break;

    default: {
        char *start = lex->cursor;

        if (m_is_digit(*token->txt.buf)) {
            token->type = M_TOKEN_NUMBER;
            while (m_is_digit(*lex->cursor) && m_advance(lex));
            if (lex->cursor != lex->end && m_is_alpha(*lex->cursor)) {
                token->type = M_TOKEN_WORD;
                while ((m_is_alpha(*lex->cursor) || m_is_digit(*lex->cursor)) && m_advance(lex));
            }
        } else if (m_is_alpha(*token->txt.buf)) {
            token->type = M_TOKEN_WORD;
            while ((m_is_alpha(*lex->cursor) || m_is_digit(*lex->cursor)) && m_advance(lex));
        } else {
            char C = *token->txt.buf;
            while (*lex->cursor == C && m_advance(lex));
        }

        token->txt.len += (int)(lex->cursor - start);
    } break;
    }
}

// =============================================================================
// @@@ Implementation / Lexer / Functions / Public
// =============================================================================
static int m_match_token    (M_Token *token, M_Token_Type type, int txt_len) { return token->type == type && token->txt.len >= txt_len; }
static int m_match_token_eq (M_Token *token, M_Token_Type type, int txt_len) { return token->type == type && token->txt.len == txt_len; }

static M_Token *m_peek_token (M_Lexer *lex, int lookahead) {
    assert(lookahead > 0 && lookahead <= M_MAX_M_TOKEN_LOOKAHEAD);
    while (lex->token_ring_item_count < lookahead) m_make_token(lex);
    int index = (lex->token_ring_cursor + lookahead - 1) & (M_MAX_M_TOKEN_LOOKAHEAD - 1);
    return &lex->token_ring[index];
}

// This an optimization; it tries to grab an entire phrase instead of a single word.
static M_Token *m_peek_token_greedy (M_Lexer *lex, int lookahead) {
    if (lex->token_ring_item_count >= lookahead) return m_peek_token(lex, lookahead);

    M_Token *token = m_peek_token(lex, lookahead);
    if (lex->cursor == lex->end) return token;
    if (token->type != M_TOKEN_WORD && token->type != M_TOKEN_WHITESPACE && token->type != M_TOKEN_NUMBER) return token;

    char C = *lex->cursor;
    while ((m_is_alpha(C) || C == ' ' || m_is_digit(C)) && m_advance(lex)) C = *lex->cursor;

    token->txt.len = (int)(lex->cursor - token->txt.buf);
    return token;
}

// We use a ring buf for tokens of length M_MAX_M_TOKEN_LOOKAHEAD > 1, so the
// returned token is valid even with 1 extra m_peek_token call after it.
static M_Token *m_eat_token (M_Lexer *lex) {
    if (lex->token_ring_item_count == 0) m_make_token(lex);

    M_Token *token = m_peek_token(lex, 1);
    if (token->type == M_TOKEN_NEWLINE) {
        lex->indentation      = -1;
        lex->at_start_of_line = 1;
    } else {
        lex->at_start_of_line = 0;
    }

    lex->token_ring_item_count--;
    lex->token_ring_cursor++;
    lex->token_ring_cursor &= M_MAX_M_TOKEN_LOOKAHEAD - 1;
    return token;
}

static int m_rest_of_line_is_empty (M_Lexer *lex) {
    M_Token_Type T = m_peek_token(lex, 1)->type;
    if (T == M_TOKEN_NEWLINE)    return 1;
    if (T != M_TOKEN_WHITESPACE) return 0;
    return (m_peek_token(lex, 2)->type == M_TOKEN_NEWLINE);
}

static int m_advance_to_next_line (M_Lexer *lex) {
    while (1) {
        switch (m_eat_token(lex)->type) {
        case M_TOKEN_EOF:     return 0;
        case M_TOKEN_NEWLINE: return 1;
        }
    }

    return 1;
}

static void m_eat_one_whitespace (M_Lexer *lex) {
    M_Token *token = m_peek_token(lex, 1);
    if (token->type != M_TOKEN_WHITESPACE) return;
    token->txt.buf++;
    if (! --token->txt.len) m_eat_token(lex);
}

static int m_eat_indentation (M_Lexer *lex, int indentation) {
    if (lex->indentation > -1) return (lex->indentation >= indentation);

    M_Token *token   = m_peek_token(lex, 1);
    lex->indentation = 0;

    if (*token->txt.buf == ' ') {
        int n = 0;
        while (*token->txt.buf == ' ' && token->txt.len && lex->indentation < indentation) {
            ++token->txt.buf; --token->txt.len;
            if (++n == 2) { n = 0; ++lex->indentation; };
        }

        if (n == 1) { --token->txt.buf; ++token->txt.len; }
    } else {
        while (*token->txt.buf == '\t' && token->txt.len && lex->indentation < indentation) {
            ++token->txt.buf; --token->txt.len; ++lex->indentation;
        }
    }

    if (token->txt.len == 0) { m_eat_token(lex); lex->at_start_of_line = 1; }
    return (lex->indentation >= indentation);
}


// =============================================================================
// @@@ Implementation / Parser / Data
// =============================================================================
#define M_MAX_TABLE_COLS      128
#define M_MAX_RECURSION_DEPTH 64

typedef struct M_Ast_Pool {
    M_Ast *nodes;
    struct M_Ast_Pool *next;
} M_Ast_Pool;

typedef enum {
    M_PARSER_STATE_NODE_ENTER,
    M_PARSER_STATE_NODE_EXIT,
    M_PARSER_STATE_PARSING_BLOCKS,
    M_PARSER_STATE_PARSING_BLOCKS_AND_CALL_BEFORE_POP,
    M_PARSER_STATE_PARSING_INLINES,

    M_PARSER_STATE_EOF,
    M_PARSER_STATE_ALLOC_FAIL,
    M_PARSER_STATE_INITIAL,
    M_PARSER_STATE_CURRENT,

    M_PARSER_STATE_TABLE,
    M_PARSER_STATE_COMMENT,
    M_PARSER_STATE_META,
    M_PARSER_STATE_LINK_REF,
    M_PARSER_STATE_LINK_TEXT,
    M_PARSER_STATE_LIST_ITEMS,
    M_PARSER_STATE_FORMAT_META,
    M_PARSER_STATE_TABLE_DONE,
    M_PARSER_STATE_TABLE_ADDING_MISSING_CELLS
} M_Parser_State;

#define M_PARSER_FLAG_NO_AST                 0x1u
#define M_PARSER_FLAG_FORMAT_MODE            0x2u
#define M_PARSER_FLAG_STRICT_FORMAT_MODE     0x4u
#define M_PARSER_FLAG_PREV_PARSED_IS_NEWLINE 0x10u

typedef struct M_Parselet_Frame M_Parselet_Frame; // forward decl
typedef M_Parser_State (*M_Parselet) (M_Parser, M_Lexer *, M_Parselet_Frame *);

struct M_Parselet_Frame {
    M_Ast *node, **node_attach_point;

    int child_indentation;
    M_Parselet parselet;
    M_Parser_State state;

    M_Token_Type delim;
    int           delim_len;

    union {
        struct { M_Token_Type first_marker; } list;
        struct M_Table_Frame { int *cell_merge_status, row, col, cols_counted; } table;
    } as;
};

struct M_Parser {
    M_Ast *root, *newest_node;

    unsigned flags;
    M_String parsed_txt;
    M_Event event;
    M_Allocators alloc;

    M_Ast_Pool *ast_pool;
    int          ast_pool_capacity, ast_pool_item_count;

    M_Parselet_Frame frames[M_MAX_RECURSION_DEPTH + 1];
    int               frames_count;

    M_Lexer lexer;
};


// =============================================================================
// @@@ Implementation / Parser / Functions / Private
// =============================================================================
static M_Parselet m_get_block_parselet (M_Parser); // forward decl
static void m_push_frame (M_Parser, M_Parselet);   // forward decl

static int m_parse_num (M_Token *token, int min, int max) {
    int tmp     = 0;
    int weight  = 1;
    char *right = token->txt.buf + token->txt.len - 1;
    char *left  = token->txt.buf;

    while (*left == '0' && left != right) left++;
    while (1) {
        tmp += (int)((*right - '0') * weight);
        if (left == right) break;
        weight *= 10; right--;
    }

    int ret = (tmp > INT_MAX) ? INT_MAX : (int)tmp;
    return (ret < min) ? min : ((ret > max) ? max : ret);
}

static void m_push_text (M_Parser parser, M_Token *token, M_Event_Type event_type) {
    if (parser->parsed_txt.buf) memcpy(parser->parsed_txt.buf + parser->parsed_txt.len, token->txt.buf, (size_t)token->txt.len);

    parser->parsed_txt.len += token->txt.len;
    if (event_type != M_EVENT_NONE) {
        parser->event.type     = event_type;
        parser->event.as.token = token;
    }
    if (token->type == M_TOKEN_NEWLINE) parser->flags |= M_PARSER_FLAG_PREV_PARSED_IS_NEWLINE;
    else parser->flags &= ~M_PARSER_FLAG_PREV_PARSED_IS_NEWLINE;
}

static M_Parser_State m_parse_text (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    (void)frame;
    m_push_text(parser, m_eat_token(lex), M_EVENT_TEXT);
    return M_PARSER_STATE_CURRENT;
}

static M_Parser_State m_parse_paragraph (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    (void)parser; (void)lex;
    frame->node->type = M_AST_PARAGRAPH;
    return M_PARSER_STATE_PARSING_INLINES;
}

static M_Parser_State m_parse_paragraph_blank (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    (void)parser;
    frame->node->type = M_AST_PARAGRAPH_BLANK;
    while (m_advance_to_next_line(lex) && m_rest_of_line_is_empty(lex));
    return M_PARSER_STATE_NODE_EXIT;
}

static M_Parser_State m_parse_link (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    switch (frame->state) {
    case M_PARSER_STATE_NODE_ENTER: {
        frame->node->type            = M_AST_LINK;
        frame->delim_len             = m_eat_token(lex)->txt.len;
        frame->node->as.link.ref.buf = parser->parsed_txt.buf ? frame->node->txt.buf : m_peek_token(lex, 1)->txt.buf;
        frame->node->as.link.ref.len = 0;
    } return M_PARSER_STATE_LINK_REF;

    case M_PARSER_STATE_LINK_REF: {
        if (lex->at_start_of_line && m_get_block_parselet(parser) != m_parse_paragraph) return M_PARSER_STATE_NODE_EXIT;
        M_Token *token = m_eat_token(lex);
        if (token->type == M_TOKEN_EOF || (m_match_token_eq(token, '>', frame->delim_len))) return M_PARSER_STATE_NODE_EXIT;

        if (m_match_token_eq(token, ':', frame->delim_len)) {
            m_eat_one_whitespace(lex);
            if (parser->parsed_txt.buf) frame->node->txt.buf += frame->node->as.link.ref.len;
            return M_PARSER_STATE_LINK_TEXT;
        }

        frame->node->as.link.ref.len += token->txt.len;
        m_push_text(parser, token, M_EVENT_TEXT_LINK_REF);
    } break;

    case M_PARSER_STATE_LINK_TEXT: {
        if (lex->at_start_of_line && m_get_block_parselet(parser) != m_parse_paragraph) return M_PARSER_STATE_NODE_EXIT;
        M_Token *token = m_eat_token(lex);
        if (token->type == M_TOKEN_EOF || (m_match_token_eq(token, '>', frame->delim_len))) return M_PARSER_STATE_NODE_EXIT;
        m_push_text(parser, token, M_EVENT_TEXT_LINK_ALIAS);
    } break;

    default: break;
    }

    return M_PARSER_STATE_CURRENT;
}

static M_Parser_State m_parse_format_inline (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    if (frame->state == M_PARSER_STATE_NODE_ENTER) {
        frame->node->type = M_AST_FORMAT_INLINE;
        m_eat_token(lex);
        return M_PARSER_STATE_FORMAT_META;
    }

    if (lex->at_start_of_line && m_get_block_parselet(parser) != m_parse_paragraph) return M_PARSER_STATE_NODE_EXIT;
    M_Token *token  = m_eat_token(lex);
    if (token->type == M_TOKEN_EOF || (m_match_token_eq(token, '`', 1))) return M_PARSER_STATE_NODE_EXIT;
    m_push_text(parser, token, M_EVENT_TEXT);
    return M_PARSER_STATE_CURRENT;
}

static M_Parser_State m_parse_emphasis (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    (void)parser;
    frame->node->type = M_AST_EMPHASIS;
    M_Token *token   = m_eat_token(lex);
    frame->delim      = token->type;
    frame->delim_len  = token->txt.len;
    frame->node->as.emphasis.lvl = (int)token->txt.len;
    return M_PARSER_STATE_PARSING_INLINES;
}

static M_Parser_State m_parse_strikethrough (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    (void)parser;
    frame->node->type = M_AST_STRIKETHROUGH;
    frame->delim      = '~';
    frame->delim_len  = m_eat_token(lex)->txt.len;
    return M_PARSER_STATE_PARSING_INLINES;
}

static M_Parser_State m_parse_sub_sup_script (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    (void)parser;
    int len = m_eat_token(lex)->txt.len;
    frame->node->type = (len == 1) ? M_AST_SUP_SCRIPT : M_AST_SUB_SCRIPT;
    frame->delim      = '^';
    frame->delim_len  = len;
    return M_PARSER_STATE_PARSING_INLINES;
}

static M_Parser_State m_parse_line_break (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    (void)parser;
    frame->node->type = M_AST_LINE_BREAK;
    m_eat_token(lex);
    return M_PARSER_STATE_NODE_EXIT;
}

static M_Parser_State m_parse_separator (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    (void)parser;
    frame->node->type = M_AST_SEPARATOR;
    m_advance_to_next_line(lex);
    return M_PARSER_STATE_NODE_EXIT;
}

static M_Parser_State m_parse_header (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    frame->node->type              = M_AST_HEADER;
    frame->node->as.header.lvl     = (int)m_eat_token(lex)->txt.len;
    frame->node->as.header.tag.buf = parser->parsed_txt.buf ? frame->node->txt.buf : m_peek_token(lex, 1)->txt.buf;
    frame->node->as.header.tag.len = 0;

    int lvl = 0;
    while (1) {
        M_Token *token = m_peek_token(lex, 1);
        if (token->type != M_TOKEN_NUMBER) break;
        lvl++;
        frame->node->as.header.tag.len += token->txt.len;
        m_push_text(parser, m_eat_token(lex), M_EVENT_NONE);

        token = m_peek_token(lex, 1);
        if (token->type != '.') break;
        frame->node->as.header.tag.len += token->txt.len;
        m_push_text(parser, m_eat_token(lex), M_EVENT_NONE);
    }

    if (lvl) frame->node->as.header.lvl = lvl;
    m_eat_one_whitespace(lex);
    return M_PARSER_STATE_PARSING_INLINES;
}

static M_Parser_State m_parse_blockquote (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    (void)parser;
    frame->node->type = M_AST_BLOCKQUOTE;
    int len = m_eat_token(lex)->txt.len;
    m_eat_one_whitespace(lex);
    if (len == 1) { frame->child_indentation++; return M_PARSER_STATE_PARSING_BLOCKS; }
    else { return M_PARSER_STATE_PARSING_INLINES; }
}

static M_Parser_State m_parse_comment (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    if (frame->state == M_PARSER_STATE_NODE_ENTER) {
        frame->node->type = M_AST_COMMENT;
        m_eat_token(lex); m_eat_one_whitespace(lex);
        return M_PARSER_STATE_COMMENT;
    } else if (lex->at_start_of_line) {
        if (m_get_block_parselet(parser) != m_parse_comment) return M_PARSER_STATE_NODE_EXIT;
        m_eat_token(lex); m_eat_one_whitespace(lex);
    } else {
        if (m_peek_token_greedy(lex, 1)->type == M_TOKEN_EOF) return M_PARSER_STATE_NODE_EXIT;
        m_push_text(parser, m_eat_token(lex), M_EVENT_TEXT_COMMENT);
    }

    return M_PARSER_STATE_CURRENT;
}

static M_Parser_State m_parse_list_item (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    (void)parser;
    M_Token *token = m_eat_token(lex);
    switch (token->type) {
    case M_TOKEN_NUMBER: frame->node->type = M_AST_LIST_BY_NUMBER_ITEM; m_eat_token(lex); break;
    case '-': frame->node->type = (token->txt.len == 2) ? M_AST_LIST_ITEM : M_AST_LIST_BY_BULLET_ITEM; break;
    case '[': frame->node->type = M_AST_LIST_BY_CHECKBOX_ITEM;
              frame->node->as.checked_list_item.is_checked = (*m_eat_token(lex)->txt.buf == 'x');
              if (m_peek_token(lex, 1)->type == ']') m_eat_token(lex);
              break;
    }

    m_eat_one_whitespace(lex);
    frame->child_indentation++;
    return M_PARSER_STATE_PARSING_BLOCKS;
}

static M_Parser_State m_parse_list (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    if (frame->state == M_PARSER_STATE_NODE_ENTER) {
        M_Token *token = m_peek_token(lex, 1);
        frame->as.list.first_marker = token->type;

        switch (token->type) {
        case M_TOKEN_NUMBER: frame->node->type = M_AST_LIST_BY_NUMBER; break;
        case '-': frame->node->type = (token->txt.len == 2) ? M_AST_LIST : M_AST_LIST_BY_BULLET; break;
        case '[': frame->node->type = M_AST_LIST_BY_CHECKBOX; break;
        default:  break;
        }

        return M_PARSER_STATE_LIST_ITEMS;
    }

    M_Parselet P = m_get_block_parselet(parser);
    if (P != m_parse_list || parser->newest_node->type == M_AST_PARAGRAPH_BLANK) return M_PARSER_STATE_NODE_EXIT;
    if (frame->as.list.first_marker != m_peek_token(lex, 1)->type) return M_PARSER_STATE_NODE_EXIT;
    m_push_frame(parser, m_parse_list_item);
    return M_PARSER_STATE_CURRENT;
}

static M_Parser_State m_parse_meta_inline (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    if (frame->state == M_PARSER_STATE_NODE_ENTER) {
        frame->node->type = M_AST_META_INLINE;
        m_eat_token(lex);
        frame->delim     = ']';
        frame->delim_len = 2;
        frame->node->as.meta.data.buf = parser->parsed_txt.buf ? frame->node->txt.buf : m_peek_token(lex, 1)->txt.buf;
        frame->node->as.meta.data.len = 0;
        return M_PARSER_STATE_META;
    }

    if (lex->at_start_of_line && m_get_block_parselet(parser) != m_parse_paragraph) return M_PARSER_STATE_NODE_EXIT;
    M_Token *token = m_eat_token(lex);
    if (token->type == '\\') { token = m_eat_token(lex); token->type = M_TOKEN_WORD; }
    if (token->type == M_TOKEN_EOF || m_match_token_eq(token, ']', 2)) return M_PARSER_STATE_NODE_EXIT;
    if (token->type == ':') { m_eat_one_whitespace(lex); parser->event.type = M_EVENT_TEXT_META_DONE; return M_PARSER_STATE_PARSING_INLINES; }

    m_push_text(parser, token, M_EVENT_TEXT_META);
    frame->node->as.meta.data.len += token->txt.len;
    return M_PARSER_STATE_CURRENT;
}

static M_Parser_State m_parse_meta_block (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    if (frame->state == M_PARSER_STATE_NODE_ENTER) {
        frame->node->type = M_AST_META_BLOCK;
        m_eat_token(lex);
        frame->delim     = ']';
        frame->delim_len = 3;
        frame->node->as.meta.data.buf = parser->parsed_txt.buf ? frame->node->txt.buf : m_peek_token(lex, 1)->txt.buf;
        frame->node->as.meta.data.len = 0;
        return M_PARSER_STATE_META;
    }

    if (lex->at_start_of_line) {
        if (!m_eat_indentation(lex, frame->child_indentation)) return M_PARSER_STATE_NODE_EXIT;
        if (m_match_token(m_peek_token(lex, 1), ']', 3)) { m_advance_to_next_line(lex); return M_PARSER_STATE_NODE_EXIT; }
    }

    M_Token *token = m_eat_token(lex);
    if (token->type == '\\') { token = m_eat_token(lex); token->type = M_TOKEN_WORD; }
    if (token->type == M_TOKEN_EOF) return M_PARSER_STATE_NODE_EXIT;
    if (token->type == ':') { m_advance_to_next_line(lex); parser->event.type = M_EVENT_TEXT_META_DONE; return M_PARSER_STATE_PARSING_BLOCKS; }

    m_push_text(parser, token, M_EVENT_TEXT_META);
    frame->node->as.meta.data.len += token->txt.len;
    return M_PARSER_STATE_CURRENT;
}

static M_Parser_State m_parse_meta_tree (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    if (frame->state == M_PARSER_STATE_NODE_ENTER) {
        frame->node->type = M_AST_META_TREE;
        frame->child_indentation++;
        m_eat_token(lex);
        frame->node->as.meta.data.buf = parser->parsed_txt.buf ? frame->node->txt.buf : m_peek_token(lex, 1)->txt.buf;
        frame->node->as.meta.data.len = 0;
        return M_PARSER_STATE_META;
    }

    M_Token *token = m_eat_token(lex);
    if (token->type == '\\') { token = m_eat_token(lex); token->type = M_TOKEN_WORD; }
    if (token->type == M_TOKEN_EOF) return M_PARSER_STATE_NODE_EXIT;
    if (token->type == M_TOKEN_NEWLINE) { parser->event.type = M_EVENT_TEXT_META_DONE; return M_PARSER_STATE_PARSING_BLOCKS; }
    if (token->type == ':') { m_eat_one_whitespace(lex); parser->event.type = M_EVENT_TEXT_META_DONE; return M_PARSER_STATE_PARSING_BLOCKS; }

    m_push_text(parser, token, M_EVENT_TEXT_META);
    frame->node->as.meta.data.len += token->txt.len;
    return M_PARSER_STATE_CURRENT;
}

static M_Parser_State m_parse_format_block (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    switch (frame->state) {
    case M_PARSER_STATE_NODE_ENTER: {
        frame->node->type = M_AST_FORMAT_BLOCK;
        frame->node->as.format_block.info.buf = parser->parsed_txt.buf ? frame->node->txt.buf : m_peek_token(lex, 1)->txt.buf;
        frame->node->as.format_block.info.len = 0;
        frame->delim     = '`';
        frame->delim_len = 3;

        m_eat_token(lex);
        if (m_peek_token(lex, 1)->type == '\\') {
            m_eat_token(lex);
            frame->node->type = M_AST_FORMAT_BLOCK_STRICT;
            parser->flags |= M_PARSER_FLAG_STRICT_FORMAT_MODE;
        } else {
            parser->flags |= M_PARSER_FLAG_FORMAT_MODE;
        }
    } return M_PARSER_STATE_FORMAT_META;

    case M_PARSER_STATE_FORMAT_META: {
        M_Token *token = m_eat_token(lex);
        if (token->type == M_TOKEN_EOF) { parser->flags &= ~(M_PARSER_FLAG_FORMAT_MODE | M_PARSER_FLAG_STRICT_FORMAT_MODE); return M_PARSER_STATE_NODE_EXIT; }
        if (token->type == M_TOKEN_NEWLINE) return M_PARSER_STATE_PARSING_BLOCKS_AND_CALL_BEFORE_POP;
        m_push_text(parser, token, M_EVENT_TEXT_META);
        frame->node->as.format_block.info.len += token->txt.len;
    } break;

    case M_PARSER_STATE_NODE_EXIT: parser->flags &= ~(M_PARSER_FLAG_FORMAT_MODE | M_PARSER_FLAG_STRICT_FORMAT_MODE); break;
    default: break;
    }

    return M_PARSER_STATE_CURRENT;
}

static void table_skip_col (M_Lexer *lex, int table_indentation) {
    while (m_advance_to_next_line(lex)) {
        if (!m_eat_indentation(lex, table_indentation) && !m_rest_of_line_is_empty(lex)) break;
        M_Token_Type T = m_peek_token(lex, 1)->type;
        if (T != M_TOKEN_WHITESPACE && T != M_TOKEN_NEWLINE) break;
    }
}

static int table_count_cols (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *table_frame) {
    struct M_Table_Frame *T = &table_frame->as.table;
    if (T->cols_counted) return table_frame->node->as.table.n_cols;
    T->cols_counted = 1;

    M_Lexer backup = *lex;
    table_frame->node->as.table.n_cols = T->col;

    while (table_frame->node->as.table.n_cols < M_MAX_TABLE_COLS) {
        table_skip_col(lex, table_frame->child_indentation);
        if (m_peek_token(lex, 1)->type != '|' || m_peek_token(lex, 2)->type == '-') break;
        table_frame->node->as.table.n_cols++;
    }

    parser->lexer = backup;
    return table_frame->node->as.table.n_cols;
}

static int table_count_rows (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *table_frame) {
    struct M_Table_Frame *T = &table_frame->as.table;
    if (table_frame->node->as.table.n_rows) return table_frame->node->as.table.n_rows;

    M_Lexer backup = *lex;
    table_frame->node->as.table.n_rows = T->row - 1;
    table_skip_col(lex, table_frame->child_indentation);

    M_Token_Type tok = M_TOKEN_EOF;
    while (m_peek_token(lex, 1)->type == '|') {
        tok = m_peek_token(lex, 2)->type;
        if (tok == '-') table_frame->node->as.table.n_rows++;
        table_skip_col(lex, table_frame->child_indentation);
    }

    if (tok != '-') table_frame->node->as.table.n_rows++; // Check whether the table ended with a '|--' delim.
    parser->lexer = backup;
    return table_frame->node->as.table.n_rows;
}

static int table_cell_is_merged (struct M_Table_Frame *T) {
    return T->cell_merge_status && (T->row < T->cell_merge_status[T->col - 1]);
}

static void table_on_row_end (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    frame->as.table.row++; frame->as.table.col = 1;
    if (m_eat_indentation(lex, frame->child_indentation) && m_peek_token(lex, 1)->type == '|') parser->event.type = M_EVENT_TABLE_ROW_END;
}

static M_Parser_State m_parse_table_cell (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    M_Parselet_Frame *table_frame = frame - 1;
    struct M_Table_Frame *T       = &table_frame->as.table;
    M_Ast *N                      = frame->node;
    N->type                        = M_AST_TABLE_CELL;
    N->as.table_cell.col           = T->col;
    N->as.table_cell.row           = T->row;
    N->as.table_cell.width         = 1;
    N->as.table_cell.height        = 1;

    if (table_frame->state == M_PARSER_STATE_TABLE_ADDING_MISSING_CELLS) { T->col++; return M_PARSER_STATE_NODE_EXIT; }

    { // Parse cell dimensions if any.
        int n_cols = 0;
        M_Token *token = m_peek_token(lex, 1);

        { // width
            if (token->type != M_TOKEN_NUMBER && !m_match_token_eq(token, '*', 1)) goto DONE;
            n_cols  = table_count_cols(parser, lex, table_frame);
            int max = n_cols - T->col + 1;
            N->as.table_cell.width = (token->type == '*') ? max : m_parse_num(token, 1, max);
            m_eat_token(lex);
        }

        if (! m_match_token_eq(m_peek_token(lex, 1), '|', 1)) goto DONE;
        m_eat_token(lex); token = m_peek_token(lex, 1);

        { // height
            if (token->type != M_TOKEN_NUMBER && !m_match_token_eq(token, '*', 1)) goto DONE;
            int n_rows = table_count_rows(parser, lex, table_frame);
            int max    = n_rows - T->row + 1;
            N->as.table_cell.height = (token->type == '*') ? max : m_parse_num(token, 1, max);
            m_eat_token(lex);
        }

DONE:   if (N->as.table_cell.height != 1 || N->as.table_cell.width != 1) {
            if (! T->cell_merge_status) T->cell_merge_status = parser->alloc.calloc((size_t)n_cols, sizeof(int));
            if (! T->cell_merge_status) return M_PARSER_STATE_ALLOC_FAIL;

            int h = T->row + N->as.table_cell.height;
            int w = N->as.table_cell.width;

            for (int i = T->col-1; (i < n_cols) && w; ++i, --w) {
                if (T->row < T->cell_merge_status[i]) { N->as.table_cell.width = i - T->col + 1; break; }
                T->cell_merge_status[i] = h;
            }
        }
    }

    if (m_match_token_eq(m_peek_token(lex, 1), ']', 1)) m_eat_token(lex);
    m_eat_one_whitespace(lex);
    T->col++;
    frame->child_indentation++;
    return M_PARSER_STATE_PARSING_BLOCKS;
}

static M_Parser_State m_parse_table (M_Parser parser, M_Lexer *lex, M_Parselet_Frame *frame) {
    struct M_Table_Frame *T = &frame->as.table;

    switch (frame->state) {
    case M_PARSER_STATE_NODE_ENTER: {
        frame->node->type            = M_AST_TABLE;
        frame->node->as.table.n_rows = 0;
        frame->node->as.table.n_cols = M_MAX_TABLE_COLS;
        T->row                       = 1;
        T->col                       = 1;
        T->cols_counted              = 0;
        T->cell_merge_status         = NULL;

        if (m_peek_token(lex, 2)->type == '-') m_advance_to_next_line(lex);
    } return M_PARSER_STATE_TABLE;

    case M_PARSER_STATE_TABLE: {
        if (!m_eat_indentation(lex, frame->child_indentation) || m_peek_token(lex, 1)->type != '|') return M_PARSER_STATE_TABLE_DONE;
        m_eat_token(lex);

        if (m_peek_token(lex, 1)->type == '-') { // End of row.
            m_advance_to_next_line(lex);

            if (T->row == 1 && frame->node->as.table.n_cols == M_MAX_TABLE_COLS) {
                T->cols_counted = 1;
                frame->node->as.table.n_cols = T->col - 1;
            } else if (T->col <= frame->node->as.table.n_cols) {
                return M_PARSER_STATE_TABLE_ADDING_MISSING_CELLS;
            }

            table_on_row_end(parser, lex, frame);
        }
        else if (T->col > frame->node->as.table.n_cols) {
            table_skip_col(lex, frame->child_indentation);
        }
        else if (table_cell_is_merged(T)) {
            table_skip_col(lex, frame->child_indentation);
            T->col++;
        }
        else {
            m_push_frame(parser, m_parse_table_cell);
        }
    } break;

    case M_PARSER_STATE_TABLE_DONE: {
        if (! T->cols_counted) frame->node->as.table.n_cols = T->col - 1;

        if      (T->col == 1) T->row--;
        else if (T->col <= frame->node->as.table.n_cols) return M_PARSER_STATE_TABLE_ADDING_MISSING_CELLS;

        frame->node->as.table.n_rows = T->row;
        parser->alloc.free(T->cell_merge_status);
        T->cell_merge_status = NULL;
    } return M_PARSER_STATE_NODE_EXIT;

    case M_PARSER_STATE_TABLE_ADDING_MISSING_CELLS: {
        if (T->col > frame->node->as.table.n_cols) { table_on_row_end(parser, lex, frame); return M_PARSER_STATE_TABLE; }
        else if (table_cell_is_merged(T)) T->col++;
        else m_push_frame(parser, m_parse_table_cell);
    } break;

    default: break;
    }

    return M_PARSER_STATE_CURRENT;
}

static M_Parselet m_get_block_parselet (M_Parser parser) {
    M_Lexer *lex = &parser->lexer;

    if (m_peek_token(lex, 1)->type == M_TOKEN_EOF) return NULL;
    if (parser->frames_count >= M_MAX_RECURSION_DEPTH) { m_eat_token(lex); return m_parse_paragraph; } // m_eat_token() prevents inf loops.
    if (m_rest_of_line_is_empty(lex)) return m_parse_paragraph_blank;
    if (lex->at_start_of_line && !m_eat_indentation(lex, parser->frames[parser->frames_count - 1].child_indentation)) return NULL;

    int S = parser->flags & M_PARSER_FLAG_STRICT_FORMAT_MODE;
    int F = S || parser->flags & M_PARSER_FLAG_FORMAT_MODE;

    M_Token *token = m_peek_token(lex, 1);
    switch (token->type) {
    case M_TOKEN_EOF:    return NULL;
    case M_TOKEN_NUMBER: return !F && (m_peek_token(lex, 2)->type == '.') ? m_parse_list : m_parse_paragraph;

    case '#': return !F                        ? m_parse_header          : m_parse_paragraph;
    case '`': return       token->txt.len >= 3 ? m_parse_format_block    : m_parse_paragraph;
    case '|': return !F && token->txt.len == 1 ? m_parse_table           : m_parse_paragraph;
    case '/': return !F && token->txt.len >= 2 ? m_parse_comment         : m_parse_paragraph;
    case '=': return !F && token->txt.len >= 3 ? m_parse_separator       : m_parse_paragraph;
    case '>': return !F && token->txt.len <= 2 ? m_parse_blockquote      : m_parse_paragraph;
    case '-': return !F && token->txt.len <= 2 ? m_parse_list            : m_parse_paragraph;
    case ':': return !S && token->txt.len == 1 ? m_parse_meta_tree       : m_parse_paragraph;
    case ']': return !S && token->txt.len >= 3 ? m_parse_paragraph_blank : m_parse_paragraph;

    case '[': {
        if (!S && token->txt.len >= 3) return m_parse_meta_block;

        if (!F && token->txt.len == 1) {
            M_Token *T2 = m_peek_token(lex, 2), *T3 = m_peek_token(lex, 3);
            if (token->txt.len != 1 || T2->txt.len != 1) break;
            if (T3->type == ']' && (*T2->txt.buf == ' ' || *T2->txt.buf == 'x')) return m_parse_list;
        }
    } break;
    }

    return m_parse_paragraph;
}

static M_Parselet m_get_inline_parselet (M_Parser parser) {
    M_Lexer *lex = &parser->lexer;
    int S        = parser->flags & M_PARSER_FLAG_STRICT_FORMAT_MODE;
    int F        = S || parser->flags & M_PARSER_FLAG_FORMAT_MODE;

    if (lex->at_start_of_line && m_get_block_parselet(parser) != m_parse_paragraph) return NULL;

    M_Token *token = F ? m_peek_token(lex, 1) : m_peek_token_greedy(lex, 1);
    if (token->type == M_TOKEN_EOF) return NULL;
    if (parser->frames_count >= M_MAX_RECURSION_DEPTH) return m_parse_text;

    switch (token->type) {
    case '\\': if (!S) { m_eat_token(lex); return m_parse_text; } break;
    case '[':  return !S && token->txt.len == 2 ? m_parse_meta_inline    : m_parse_text;
    case '*':  return !F                        ? m_parse_emphasis       : m_parse_text;
    case '^':  return !F                        ? m_parse_sub_sup_script : m_parse_text;
    case '`':  return !F                        ? m_parse_format_inline  : m_parse_text;
    case '<':  return !F && token->txt.len >= 2 ? m_parse_link           : m_parse_text;
    case '~':  return !F && token->txt.len == 2 ? m_parse_strikethrough  : m_parse_text;
    case '|':  return !F && m_peek_token(lex, 2)->type == M_TOKEN_NEWLINE ? m_parse_line_break : m_parse_text;
    }

    return m_parse_text;
}

static int m_make_node_pool (M_Parser parser) {
    M_Ast_Pool *pool = parser->alloc.malloc(sizeof(*pool));
    if (! pool) return 0;

    pool->nodes      = parser->alloc.calloc((size_t)parser->ast_pool_capacity, sizeof(M_Ast));
    pool->next       = parser->ast_pool;
    parser->ast_pool = pool;
    parser->ast_pool_item_count = 0;
    return (pool->nodes != NULL);
}

static M_Ast *m_make_node (M_Parser parser) {
    if (parser->ast_pool_item_count == parser->ast_pool_capacity && !m_make_node_pool(parser)) return NULL;

    M_Token *token     = m_peek_token(&parser->lexer, 1);
    M_Ast *node        = &parser->ast_pool->nodes[parser->ast_pool_item_count++];
    node->txt.buf       = parser->parsed_txt.buf ? (parser->parsed_txt.buf + parser->parsed_txt.len) : token->txt.buf;
    node->start_line    = token->line;
    node->first_child   = NULL;
    node->right_sibling = NULL;
    parser->newest_node = node;
    return node;
}

static void m_push_frame (M_Parser parser, M_Parselet parselet) {
    assert(parser->frames_count <= M_MAX_RECURSION_DEPTH);

    M_Parselet_Frame *frame        = &parser->frames[parser->frames_count++];
    M_Parselet_Frame *parent_frame = frame - 1;
    assert(parent_frame->state != M_PARSER_STATE_NODE_ENTER);

    M_Ast *node = m_make_node(parser);
    if (! node) { frame->state = M_PARSER_STATE_ALLOC_FAIL; return; }

    *parent_frame->node_attach_point = node;
    parent_frame->node_attach_point  = &node->right_sibling;

    frame->node              = node;
    frame->parselet          = parselet;
    frame->node_attach_point = &node->first_child;
    frame->delim             = M_TOKEN_EOF;
    frame->child_indentation = parent_frame->child_indentation;
    frame->state             = M_PARSER_STATE_NODE_ENTER;
    frame->state             = parselet(parser, &parser->lexer, frame);

    parser->event.type    = M_EVENT_NODE_ENTER;
    parser->event.as.node = node;
}

static void m_pop_frame (M_Parser parser) {
    if (parser->frames_count == 1) {
        parser->event.type = M_EVENT_EOF;
        parser->frames[0].state = M_PARSER_STATE_EOF;
        return;
    }

    M_Parselet_Frame *frame = &parser->frames[--parser->frames_count];

    if (frame->state == M_PARSER_STATE_PARSING_BLOCKS_AND_CALL_BEFORE_POP) {
        frame->state = M_PARSER_STATE_NODE_EXIT;
        frame->parselet(parser, &parser->lexer, frame);
        if (frame->state == M_PARSER_STATE_ALLOC_FAIL) return;
    }

    frame->node->txt.len  = parser->parsed_txt.buf ? (int)(parser->parsed_txt.buf + parser->parsed_txt.len - frame->node->txt.buf) : 0;
    frame->node->end_line = m_peek_token(&parser->lexer, 1)->line;
    if (parser->flags & M_PARSER_FLAG_PREV_PARSED_IS_NEWLINE) frame->node->end_line--;
    if (parser->flags & M_PARSER_FLAG_NO_AST) parser->ast_pool_item_count--;

    parser->event.type    = M_EVENT_NODE_EXIT;
    parser->event.as.node = frame->node;
}


// =============================================================================
// @@@ Implementation / Parser / Functions / Public
// =============================================================================
M_Parser m_new_with_alloc (char *buf, int buf_len, int make_ast, M_Allocators alloc) {
    M_Parser parser = alloc.calloc(1, sizeof(*parser));
    if (! parser) return NULL;

    if (make_ast) {
        parser->ast_pool_capacity = (buf_len < 800) ? 8 : (buf_len / 100);
        parser->parsed_txt.buf    = alloc.malloc((size_t)buf_len);
        if (! parser->parsed_txt.buf) { alloc.free(parser); return NULL; }
    } else {
        parser->flags |= M_PARSER_FLAG_NO_AST;
        parser->ast_pool_capacity = M_MAX_RECURSION_DEPTH + 1; // The one pool will be used as a stack.
    }

    parser->alloc                       = alloc;
    parser->lexer.start                 = buf;
    parser->lexer.end                   = buf + buf_len;
    parser->lexer.cursor                = buf;
    parser->lexer.line                  = 1;
    parser->lexer.column                = 1;
    parser->lexer.indentation           = -1;
    parser->lexer.last_line_has_newline = (buf[buf_len - 1] == '\n');

    // Push the root frame "manually" for convenience. No enter/exit events are emitted for this node.
    {
        if (! m_make_node_pool(parser)) { m_free(parser); return NULL; }

        M_Parselet_Frame *frame = &parser->frames[parser->frames_count++];
        frame->node = m_make_node(parser);
        if (! frame->node) { m_free(parser); return NULL; }

        frame->state             = M_PARSER_STATE_PARSING_BLOCKS;
        parser->root             = frame->node;
        frame->node->type        = M_AST_ROOT;
        frame->node_attach_point = &frame->node->first_child;
    }

    return parser;
}

M_Parser m_new (char *buf, int buf_len, int make_ast) {
    M_Allocators alloc = { malloc, calloc, free };
    return m_new_with_alloc(buf, buf_len, make_ast, alloc);
}

M_Ast *m_get_root (M_Parser parser) {
    return parser->root;
}

void m_free (M_Parser parser) {
    M_Ast_Pool *pool = parser->ast_pool;
    while (pool) {
        M_Ast_Pool *tmp = pool->next;
        parser->alloc.free(pool->nodes);
        parser->alloc.free(pool);
        pool = tmp;
    }

    // If needed, we can make a free list for all heap allocs to make this cleaner.
    for (int i = 0 ; i < parser->frames_count; ++i) {
        M_Parselet_Frame *frame = &parser->frames[i];
        if (frame->parselet == m_parse_table) parser->alloc.free(frame->as.table.cell_merge_status);
    }

    parser->alloc.free(parser->parsed_txt.buf);
    parser->alloc.free(parser);
}

M_Event *m_next (M_Parser parser) {
    assert(parser->frames_count > 0);

    M_Lexer *lex            = &parser->lexer;
    M_Parselet_Frame *frame = &parser->frames[parser->frames_count - 1];
    parser->event.type       = M_EVENT_NONE;

REPEAT_SWITCH:
    switch (frame->state) {
    case M_PARSER_STATE_EOF:       parser->event.type = M_EVENT_EOF; break;
    case M_PARSER_STATE_NODE_EXIT: m_pop_frame(parser); break;

    case M_PARSER_STATE_PARSING_BLOCKS:
    case M_PARSER_STATE_PARSING_BLOCKS_AND_CALL_BEFORE_POP: {
        M_Parselet P = m_get_block_parselet(parser);
        if (! P) { m_pop_frame(parser); }
        else if (m_match_token(m_peek_token(lex, 1), frame->delim, frame->delim_len)) { m_pop_frame(parser); m_advance_to_next_line(lex); }
        else { m_push_frame(parser, P); }
    } break;

    case M_PARSER_STATE_PARSING_INLINES: {
        M_Parselet P = m_get_inline_parselet(parser);
        if (! P) m_pop_frame(parser);
        else if (m_match_token_eq(m_peek_token(lex, 1), frame->delim, frame->delim_len)) { m_pop_frame(parser); m_eat_token(lex); }
        else if (P == m_parse_text) P(parser, &parser->lexer, frame);
        else m_push_frame(parser, P);
    } break;

    default: {
        M_Parser_State S = frame->parselet(parser, lex, frame);
        int parselet_has_pushed_frame = (frame != &parser->frames[parser->frames_count - 1]);
        if (S == M_PARSER_STATE_CURRENT || parselet_has_pushed_frame) break;
        else if (S == M_PARSER_STATE_NODE_EXIT) m_pop_frame(parser);
        else frame->state = S;

        if (parser->event.type == M_EVENT_NONE) goto REPEAT_SWITCH;
    } break;
    }

    if (frame->state == M_PARSER_STATE_ALLOC_FAIL) parser->event.type = M_EVENT_ALLOC_FAIL;
    parser->event.indentation = frame->child_indentation;
    return &parser->event;
}

#endif // MARKUP_IMPLEMENTATION
