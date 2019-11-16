// =============================================================================
// @@@ Info
//
// License is at the bottom of the file.
//
// This is an ISO C99 parser for the pnakotic markup language. Documentation
// of the markup language is not included here.
//
// Dependencies: <stdlib.h> <string.h> <assert.h> <limits.h>.
//
// More info is available in the @@@ Header sections as well.
//
// -----------------------------------------------------------------------------
// INCLUDING:
//
// In one of your files do:
//     #define PNAKOTIC_IMPLEMENTATION
//     #include "pnakotic.h"
//
// In all other files just do an #include to get the header.
//
//
// -----------------------------------------------------------------------------
// EXAMPLE (WITHOUT MAKING AN AST):
//
//    Pn_Parser *pn = pn_new(input_buf, input_buf_len, make_ast=0);
//
//    while (1) {
//        Pn_Event *e = pn_next(pn);
//    }
//
//    pn_free(pn);
//
// About the input_buf:
//   - It must be UTF-8 encoded.
//   - It doesn't need to be NUL-terminated.
//   - Lines can end with a single '\n' char or the sequence '\r\n'.
//   - Last line doesn't need to terminated with a newline char.
//
//
// -----------------------------------------------------------------------------
// EXAMPLE (MAKING AN AST):
//
//    Pn_Parser *pn = pn_new(input_buf, input_buf_len, make_ast=1);
//    Pn_Ast *root  = pn_get_root(pn);
//
//    // calls to pn_next will keep expanding the tree now.
//
//    pn_free(pn); // this also frees the tree.
//
//
// -----------------------------------------------------------------------------
// MEMORY:
//
// When NOT building an AST:
//   - Pn_Ast nodes exist until you receive the PN_EVENT_NODE_EXIT.
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
//   - You can supply custom allocators with pn_new_with_alloc().
//   - You only have to do pn_free().
//   - Pn_Event and Pn_Token exist until the next call to pn_next().
//   - If a table uses cell merging, then it will temporarily heap allocate an
//     array of length equal to the number of columns. We're being lazy and
//     hardcoding the max number of columns...
// =============================================================================


// =============================================================================
// @@@ Header
// =============================================================================
#ifndef PNAKOTIC_HEADER
#define PNAKOTIC_HEADER


// =============================================================================
// @@@ Header / Data
// =============================================================================
typedef struct {
    void *(*malloc) (size_t); // These should behave like the regular C funcs.
    void *(*calloc) (size_t, size_t);
    void  (*free)   (void *);
} Pn_Allocators;

typedef struct {
    char *buf; // Not NUL-terminated!
    int   len;
} Pn_String;

typedef int Pn_Token_Type;
enum {
    // Use ascii chars for token types directly.
    PN_TOKEN_WORD = 128, // Sequence of a-zA-Z or any multibyte unicode char.
    PN_TOKEN_NUMBER,     // Sequence of digits 0-9, but only if not next to a word. "1a" or "a1" are words.
    PN_TOKEN_WHITESPACE, // Space or tab.
    PN_TOKEN_NEWLINE,    // The single char '\n' or 2 char sequence "\r\n".
    PN_TOKEN_EOF         // Doesn't correspond to a char. Means end of file.
};

typedef struct {
    Pn_String txt;
    Pn_Token_Type type;
    int line, column;
} Pn_Token;

typedef int Pn_Ast_Type;
enum {
    PN_AST_ROOT,
    PN_AST_PARAGRAPH,
    PN_AST_PARAGRAPH_BLANK,
    PN_AST_TABLE,
    PN_AST_TABLE_CELL,
    PN_AST_LIST,
    PN_AST_LIST_ITEM,
    PN_AST_LIST_BY_BULLET,
    PN_AST_LIST_BY_BULLET_ITEM,
    PN_AST_LIST_BY_NUMBER,
    PN_AST_LIST_BY_NUMBER_ITEM,
    PN_AST_LIST_BY_CHECKBOX,
    PN_AST_LIST_BY_CHECKBOX_ITEM,
    PN_AST_FORMAT_BLOCK,
    PN_AST_FORMAT_BLOCK_STRICT,
    PN_AST_FORMAT_INLINE,
    PN_AST_META_TREE,
    PN_AST_META_BLOCK,
    PN_AST_META_INLINE,
    PN_AST_HEADER,
    PN_AST_COMMENT,
    PN_AST_SEPARATOR,
    PN_AST_BLOCKQUOTE,
    PN_AST_LINK,
    PN_AST_EMPHASIS,
    PN_AST_LINE_BREAK,
    PN_AST_SUP_SCRIPT,
    PN_AST_SUB_SCRIPT,
    PN_AST_STRIKETHROUGH
};

// Since we only do 1 pass on the input, a node will not have all the info
// available when you get to the PN_EVENT_NODE_ENTER event.
//
// For example:
//   - start_line is available at node enter, and end_line at node_exit.
//   - link.ref is available right after you parsed it (when you get the
//     first PN_EVENT_TEXT_LINK_ALIAS or a PN_EVENT_NODE_EXIT).
//
// The txt prop is a little tricky:
//   - If you're not building an AST, then don't use it, just rely on the
//     stream of tokens.
//   - If you're dealing with blocks that contain text directly (like
//     paragraphs, or headers) then this is a pointer to an internal buffer
//     containing that text.  All delimiters and indentation are removed.
//     For example the txt of the paragaph inside this bullet node:
//         "- Here is the *paragraph*."
//     becomes:
//         "Here is the paragraph."
//   - If it's a link node, then this is the alias. That is, in the link
//     "<<www.whatever.com:: Whatever>> it's the text "Whatever".
//   - For nodes that don't directly contain text like list nodes, table cells,
//     etc..., this prop is largely useless, it's the combined text of all nodes
//     inside of it.
typedef struct Pn_Ast {
    struct Pn_Ast *first_child, *right_sibling; // Don't use if not making an AST.
    int start_line, end_line;
    Pn_String txt; // Don't use if not making an AST.

    Pn_Ast_Type type;
    union {
        struct { Pn_String ref; }               link;              // Available after parsing ref.
        struct { Pn_String data; }              meta;              // After parsing metadata.
        struct { Pn_String info; }              format_block;      // After parsing info string.
        struct { Pn_String tag; int lvl; }      header;            // At enter.
        struct { int lvl; }                     emphasis;          // At enter.
        struct { int is_checked; }              checked_list_item; // At enter.
        struct { int n_rows, n_cols; }          table;             // At exit.
        struct { int row, col, width, height; } table_cell;        // row/col at enter, width/height at exit.
    } as;
} Pn_Ast;

// An event either has a node/token payload, or no payload.
//
// Most ascii chars will always be returned as standalone tokens, and a sequence
// of the same char is always combined into 1 token (with exception of
// backslashes '\') For example: (;) (--) (!!!) (**), etc...
//
// The newline chars are always returned as 1 token PN_TOKEN_NEWLINE.
//
// When it comes to letter, digits, spaces/tabs, and unicode multibyte chars;
// depending on the context they will either be returned as standalone tokens or
// merged into 1 token (a phrase):
//   - When you're inside a format node or parsing the data of a meta node (or
//     others; see bellow), then the above chars are always returned as standalone
//     tokens (PN_TOKEN_WORD, PN_TOKEN_NUMBER, PN_TOKEN_WHITESPACE, PN_TOKEN_NEWLINE)
//     so that this library can be more easily used as a lexer.
//   - When parsing regular text (like paragraphs, headers, etc..) the above will be
//     merged into 1 token when possible. That is, do not rely on the type of the
//     token here unless it's an ascii punctuation.
typedef enum {
    PN_EVENT_NODE_ENTER,      // node.
    PN_EVENT_NODE_EXIT,       // node.
    PN_EVENT_TEXT,            // token. returns phrases if not inside a format node.
    PN_EVENT_TEXT_COMMENT,    // token. returns phrases.
    PN_EVENT_TEXT_LINK_REF,   // token. returns proper tokens. <<link_ref:: link_alias>>.
    PN_EVENT_TEXT_LINK_ALIAS, // token. returns proper tokens.
    PN_EVENT_TEXT_META,       // token. returns proper tokens.
    PN_EVENT_TEXT_META_DONE,  // No payload. Means no more meatadata text.
    PN_EVENT_TABLE_ROW_END,   // No payload.
    PN_EVENT_EOF,             // No payload.
    PN_EVENT_ALLOC_FAIL,      // No payload.
    PN_EVENT_NONE             // Never emitted; used internally.
} Pn_Event_Type;

typedef struct {
    int indentation;
    Pn_Event_Type type;
    union { Pn_Ast *node; Pn_Token *token; } as;
} Pn_Event;

typedef struct Pn_Parser *Pn_Parser;


// =============================================================================
// @@@ Header / Functions
// =============================================================================
Pn_Parser  pn_new_with_alloc (char *buf, int buf_len, int make_ast, Pn_Allocators);
Pn_Parser  pn_new            (char *buf, int buf_len, int make_ast);
void       pn_free           (Pn_Parser);
Pn_Ast    *pn_get_root       (Pn_Parser);
Pn_Event  *pn_next           (Pn_Parser);

#endif // PNAKOTIC_HEADER


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
#ifdef PNAKOTIC_IMPLEMENTATION

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>


// =============================================================================
// @@@ Implementation / Lexer / Data
// =============================================================================
#define PN_MAX_PN_TOKEN_LOOKAHEAD 4 // Keep this a power of 2.

typedef struct {
    char *start, *end, *cursor;
    int  line, column, indentation;

    char last_line_has_newline;
    char at_start_of_line; // Doesn't mean column=1; it means we didn't eat any tokens after the indentation.

    Pn_Token token_ring[PN_MAX_PN_TOKEN_LOOKAHEAD];
    int      token_ring_cursor;
    int      token_ring_item_count;
} Pn_Lexer;


// =============================================================================
// @@@ Implementation / Lexer / Functions / Private
// =============================================================================
#define pn_is_digit(C) (C >= '0' && C <= '9')
#define pn_is_alpha(C) ((C >= 'a' && C <= 'z') || C == '_' || (unsigned char)C > 127 || (C >= 'A' && C <= 'Z'))

static int pn_advance (Pn_Lexer *lex) {
    if (lex->cursor == lex->end) return 0;
    lex->column++;
    if (*lex->cursor++ == '\n') { lex->line++; lex->column = 1; }
    return 1;
}

static void pn_make_token (Pn_Lexer *lex) {
    int index       = (lex->token_ring_cursor + lex->token_ring_item_count) & (PN_MAX_PN_TOKEN_LOOKAHEAD - 1);
    Pn_Token *token = &lex->token_ring[index];
    token->txt.buf  = lex->cursor;
    token->txt.len  = 1;
    token->type     = *token->txt.buf;
    token->line     = lex->line;
    token->column   = lex->column;

    lex->token_ring_item_count++;
    if (lex->cursor == lex->end) { token->type = PN_TOKEN_EOF; return; }
    pn_advance(lex);

    switch (*token->txt.buf) {
    case '\\': break; // Keep the backslash a 1 char token for escaping.
    case '\n': token->type = PN_TOKEN_NEWLINE; break;
    case '\r': if (*lex->cursor == '\n') { token->type = PN_TOKEN_NEWLINE; token->txt.len++; pn_advance(lex); } break;

    case ' ': case '\t': {
        token->type = PN_TOKEN_WHITESPACE;
        char *start = lex->cursor;
        while ((*lex->cursor == ' ' || *lex->cursor == '\t') && pn_advance(lex));
        token->txt.len += (int)(lex->cursor - start);
   } break;

    default: {
        char *start = lex->cursor;

        if (pn_is_digit(*token->txt.buf)) {
            token->type = PN_TOKEN_NUMBER;
            while (pn_is_digit(*lex->cursor) && pn_advance(lex));
            if (lex->cursor != lex->end && pn_is_alpha(*lex->cursor)) {
                token->type = PN_TOKEN_WORD;
                while ((pn_is_alpha(*lex->cursor) || pn_is_digit(*lex->cursor)) && pn_advance(lex));
            }
        } else if (pn_is_alpha(*token->txt.buf)) {
            token->type = PN_TOKEN_WORD;
            while ((pn_is_alpha(*lex->cursor) || pn_is_digit(*lex->cursor)) && pn_advance(lex));
        } else {
            char C = *token->txt.buf;
            while (*lex->cursor == C && pn_advance(lex));
        }

        token->txt.len += (int)(lex->cursor - start);
    } break;
    }
}

// =============================================================================
// @@@ Implementation / Lexer / Functions / Public
// =============================================================================
static int pn_match_token    (Pn_Token *token, Pn_Token_Type type, int txt_len) { return token->type == type && token->txt.len >= txt_len; }
static int pn_match_token_eq (Pn_Token *token, Pn_Token_Type type, int txt_len) { return token->type == type && token->txt.len == txt_len; }

static Pn_Token *pn_peek_token (Pn_Lexer *lex, int lookahead) {
    assert(lookahead > 0 && lookahead <= PN_MAX_PN_TOKEN_LOOKAHEAD);
    while (lex->token_ring_item_count < lookahead) pn_make_token(lex);
    int index = (lex->token_ring_cursor + lookahead - 1) & (PN_MAX_PN_TOKEN_LOOKAHEAD - 1);
    return &lex->token_ring[index];
}

// This an optimization; it tries to grab an entire phrase instead of a single word.
static Pn_Token *pn_peek_token_greedy (Pn_Lexer *lex, int lookahead) {
    if (lex->token_ring_item_count >= lookahead) return pn_peek_token(lex, lookahead);

    Pn_Token *token = pn_peek_token(lex, lookahead);
    if (lex->cursor == lex->end) return token;
    if (token->type != PN_TOKEN_WORD && token->type != PN_TOKEN_WHITESPACE && token->type != PN_TOKEN_NUMBER) return token;

    char C = *lex->cursor;
    while ((pn_is_alpha(C) || C == ' ' || pn_is_digit(C)) && pn_advance(lex)) C = *lex->cursor;

    token->txt.len = (int)(lex->cursor - token->txt.buf);
    return token;
}

// We use a ring buf for tokens of length PN_MAX_PN_TOKEN_LOOKAHEAD > 1, so the
// returned token is valid even with 1 extra pn_peek_token call after it.
static Pn_Token *pn_eat_token (Pn_Lexer *lex) {
    if (lex->token_ring_item_count == 0) pn_make_token(lex);

    Pn_Token *token = pn_peek_token(lex, 1);
    if (token->type == PN_TOKEN_NEWLINE) {
        lex->indentation      = -1;
        lex->at_start_of_line = 1;
    } else {
        lex->at_start_of_line = 0;
    }

    lex->token_ring_item_count--;
    lex->token_ring_cursor++;
    lex->token_ring_cursor &= PN_MAX_PN_TOKEN_LOOKAHEAD - 1;
    return token;
}

static int pn_rest_of_line_is_empty (Pn_Lexer *lex) {
    Pn_Token_Type T = pn_peek_token(lex, 1)->type;
    if (T == PN_TOKEN_NEWLINE)    return 1;
    if (T != PN_TOKEN_WHITESPACE) return 0;
    return (pn_peek_token(lex, 2)->type == PN_TOKEN_NEWLINE);
}

static int pn_advance_to_next_line (Pn_Lexer *lex) {
    while (1) {
        switch (pn_eat_token(lex)->type) {
        case PN_TOKEN_EOF:     return 0;
        case PN_TOKEN_NEWLINE: return 1;
        }
    }
    return 1;
}

static void pn_eat_one_whitespace (Pn_Lexer *lex) {
    Pn_Token *token = pn_peek_token(lex, 1);
    if (token->type != PN_TOKEN_WHITESPACE) return;
    token->txt.buf++;
    if (! --token->txt.len) pn_eat_token(lex);
}

static int pn_eat_indentation (Pn_Lexer *lex, int indentation) {
    if (lex->indentation > -1) return (lex->indentation >= indentation);

    Pn_Token *token  = pn_peek_token(lex, 1);
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

    if (token->txt.len == 0) { pn_eat_token(lex); lex->at_start_of_line = 1; }
    return (lex->indentation >= indentation);
}


// =============================================================================
// @@@ Implementation / Parser / Data
// =============================================================================
#define PN_MAX_TABLE_COLS      128
#define PN_MAX_RECURSION_DEPTH 64

typedef struct Pn_Ast_Pool {
    Pn_Ast *nodes;
    struct Pn_Ast_Pool *next;
} Pn_Ast_Pool;

typedef enum {
    PN_PARSER_STATE_NODE_ENTER,
    PN_PARSER_STATE_NODE_EXIT,
    PN_PARSER_STATE_PARSING_BLOCKS,
    PN_PARSER_STATE_PARSING_BLOCKS_AND_CALL_BEFORE_POP,
    PN_PARSER_STATE_PARSING_INLINES,

    PN_PARSER_STATE_EOF,
    PN_PARSER_STATE_ALLOC_FAIL,
    PN_PARSER_STATE_INITIAL,
    PN_PARSER_STATE_CURRENT,

    PN_PARSER_STATE_TABLE,
    PN_PARSER_STATE_COMMENT,
    PN_PARSER_STATE_META,
    PN_PARSER_STATE_LINK_REF,
    PN_PARSER_STATE_LINK_TEXT,
    PN_PARSER_STATE_LIST_ITEMS,
    PN_PARSER_STATE_FORMAT_META,
    PN_PARSER_STATE_TABLE_DONE,
    PN_PARSER_STATE_TABLE_ADDING_MISSING_CELLS
} Pn_Parser_State;

#define PN_PARSER_FLAG_NO_AST                 0x1u
#define PN_PARSER_FLAG_FORMAT_MODE            0x2u
#define PN_PARSER_FLAG_STRICT_FORMAT_MODE     0x4u
#define PN_PARSER_FLAG_PREV_PARSED_IS_NEWLINE 0x10u

typedef struct Pn_Parselet_Frame Pn_Parselet_Frame; // forward decl
typedef Pn_Parser_State (*Pn_Parselet) (Pn_Parser, Pn_Lexer *, Pn_Parselet_Frame *);

struct Pn_Parselet_Frame {
    Pn_Ast *node, **node_attach_point;

    int child_indentation;
    Pn_Parselet parselet;
    Pn_Parser_State state;

    Pn_Token_Type delim;
    int           delim_len;

    union {
        struct { Pn_Token_Type first_marker; } list;
        struct Pn_Table_Frame { int *cell_merge_status, row, col, cols_counted; } table;
    } as;
};

struct Pn_Parser {
    Pn_Ast *root, *newest_node;

    unsigned flags;
    Pn_String parsed_txt;
    Pn_Event event;
    Pn_Allocators alloc;

    Pn_Ast_Pool *ast_pool;
    int          ast_pool_capacity, ast_pool_item_count;

    Pn_Parselet_Frame frames[PN_MAX_RECURSION_DEPTH + 1];
    int               frames_count;

    Pn_Lexer lexer;
};


// =============================================================================
// @@@ Implementation / Parser / Functions / Private
// =============================================================================
static Pn_Parselet pn_get_block_parselet (Pn_Parser); // forward decl
static void pn_push_frame (Pn_Parser, Pn_Parselet);   // forward decl

static int pn_parse_num (Pn_Token *token, int min, int max) {
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

static void pn_push_text (Pn_Parser parser, Pn_Token *token, Pn_Event_Type event_type) {
    if (parser->parsed_txt.buf) memcpy(parser->parsed_txt.buf + parser->parsed_txt.len, token->txt.buf, (size_t)token->txt.len);

    parser->parsed_txt.len += token->txt.len;
    if (event_type != PN_EVENT_NONE) {
        parser->event.type     = event_type;
        parser->event.as.token = token;
    }
    if (token->type == PN_TOKEN_NEWLINE) parser->flags |= PN_PARSER_FLAG_PREV_PARSED_IS_NEWLINE;
    else parser->flags &= ~PN_PARSER_FLAG_PREV_PARSED_IS_NEWLINE;
}

static Pn_Parser_State pn_parse_text (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    (void)frame;
    pn_push_text(parser, pn_eat_token(lex), PN_EVENT_TEXT);
    return PN_PARSER_STATE_CURRENT;
}

static Pn_Parser_State pn_parse_paragraph (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    (void)parser; (void)lex;
    frame->node->type = PN_AST_PARAGRAPH;
    return PN_PARSER_STATE_PARSING_INLINES;
}

static Pn_Parser_State pn_parse_paragraph_blank (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    (void)parser;
    frame->node->type = PN_AST_PARAGRAPH_BLANK;
    while (pn_advance_to_next_line(lex) && pn_rest_of_line_is_empty(lex));
    return PN_PARSER_STATE_NODE_EXIT;
}

static Pn_Parser_State pn_parse_link (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    switch (frame->state) {
    case PN_PARSER_STATE_NODE_ENTER: {
        frame->node->type            = PN_AST_LINK;
        frame->delim_len             = pn_eat_token(lex)->txt.len;
        frame->node->as.link.ref.buf = parser->parsed_txt.buf ? frame->node->txt.buf : pn_peek_token(lex, 1)->txt.buf;
        frame->node->as.link.ref.len = 0;
    } return PN_PARSER_STATE_LINK_REF;

    case PN_PARSER_STATE_LINK_REF: {
        if (lex->at_start_of_line && pn_get_block_parselet(parser) != pn_parse_paragraph) return PN_PARSER_STATE_NODE_EXIT;
        Pn_Token *token = pn_eat_token(lex);
        if (token->type == PN_TOKEN_EOF || (pn_match_token_eq(token, '>', frame->delim_len))) return PN_PARSER_STATE_NODE_EXIT;

        if (pn_match_token_eq(token, ':', frame->delim_len)) {
            pn_eat_one_whitespace(lex);
            if (parser->parsed_txt.buf) frame->node->txt.buf += frame->node->as.link.ref.len;
            return PN_PARSER_STATE_LINK_TEXT;
        }

        frame->node->as.link.ref.len += token->txt.len;
        pn_push_text(parser, token, PN_EVENT_TEXT_LINK_REF);
    } break;

    case PN_PARSER_STATE_LINK_TEXT: {
        if (lex->at_start_of_line && pn_get_block_parselet(parser) != pn_parse_paragraph) return PN_PARSER_STATE_NODE_EXIT;
        Pn_Token *token = pn_eat_token(lex);
        if (token->type == PN_TOKEN_EOF || (pn_match_token_eq(token, '>', frame->delim_len))) return PN_PARSER_STATE_NODE_EXIT;
        pn_push_text(parser, token, PN_EVENT_TEXT_LINK_ALIAS);
    } break;

    default: break;
    }

    return PN_PARSER_STATE_CURRENT;
}

static Pn_Parser_State pn_parse_format_inline (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    if (frame->state == PN_PARSER_STATE_NODE_ENTER) {
        frame->node->type = PN_AST_FORMAT_INLINE;
        pn_eat_token(lex);
        return PN_PARSER_STATE_FORMAT_META;
    }

    if (lex->at_start_of_line && pn_get_block_parselet(parser) != pn_parse_paragraph) return PN_PARSER_STATE_NODE_EXIT;
    Pn_Token *token  = pn_eat_token(lex);
    if (token->type == PN_TOKEN_EOF || (pn_match_token_eq(token, '`', 1))) return PN_PARSER_STATE_NODE_EXIT;
    pn_push_text(parser, token, PN_EVENT_TEXT);
    return PN_PARSER_STATE_CURRENT;
}

static Pn_Parser_State pn_parse_emphasis (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    (void)parser;
    frame->node->type = PN_AST_EMPHASIS;
    Pn_Token *token   = pn_eat_token(lex);
    frame->delim      = token->type;
    frame->delim_len  = token->txt.len;
    frame->node->as.emphasis.lvl = (int)token->txt.len;
    return PN_PARSER_STATE_PARSING_INLINES;
}

static Pn_Parser_State pn_parse_strikethrough (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    (void)parser;
    frame->node->type = PN_AST_STRIKETHROUGH;
    frame->delim      = '~';
    frame->delim_len  = pn_eat_token(lex)->txt.len;
    return PN_PARSER_STATE_PARSING_INLINES;
}

static Pn_Parser_State pn_parse_sub_sup_script (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    (void)parser;
    int len = pn_eat_token(lex)->txt.len;
    frame->node->type = (len == 1) ? PN_AST_SUP_SCRIPT : PN_AST_SUB_SCRIPT;
    frame->delim      = '^';
    frame->delim_len  = len;
    return PN_PARSER_STATE_PARSING_INLINES;
}

static Pn_Parser_State pn_parse_line_break (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    (void)parser;
    frame->node->type = PN_AST_LINE_BREAK;
    pn_eat_token(lex);
    return PN_PARSER_STATE_NODE_EXIT;
}

static Pn_Parser_State pn_parse_separator (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    (void)parser;
    frame->node->type = PN_AST_SEPARATOR;
    pn_advance_to_next_line(lex);
    return PN_PARSER_STATE_NODE_EXIT;
}

static Pn_Parser_State pn_parse_header (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    frame->node->type              = PN_AST_HEADER;
    frame->node->as.header.lvl     = (int)pn_eat_token(lex)->txt.len;
    frame->node->as.header.tag.buf = parser->parsed_txt.buf ? frame->node->txt.buf : pn_peek_token(lex, 1)->txt.buf;
    frame->node->as.header.tag.len = 0;

    int lvl = 0;
    while (1) {
        Pn_Token *token = pn_peek_token(lex, 1);
        if (token->type != PN_TOKEN_NUMBER) break;
        lvl++;
        frame->node->as.header.tag.len += token->txt.len;
        pn_push_text(parser, pn_eat_token(lex), PN_EVENT_NONE);

        token = pn_peek_token(lex, 1);
        if (token->type != '.') break;
        frame->node->as.header.tag.len += token->txt.len;
        pn_push_text(parser, pn_eat_token(lex), PN_EVENT_NONE);
    }

    if (lvl) frame->node->as.header.lvl = lvl;
    pn_eat_one_whitespace(lex);
    return PN_PARSER_STATE_PARSING_INLINES;
}

static Pn_Parser_State pn_parse_blockquote (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    (void)parser;
    frame->node->type = PN_AST_BLOCKQUOTE;
    int len = pn_eat_token(lex)->txt.len;
    pn_eat_one_whitespace(lex);
    if (len == 1) { frame->child_indentation++; return PN_PARSER_STATE_PARSING_BLOCKS; }
    else { return PN_PARSER_STATE_PARSING_INLINES; }
}

static Pn_Parser_State pn_parse_comment (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    if (frame->state == PN_PARSER_STATE_NODE_ENTER) {
        frame->node->type = PN_AST_COMMENT;
        pn_eat_token(lex); pn_eat_one_whitespace(lex);
        return PN_PARSER_STATE_COMMENT;
    } else if (lex->at_start_of_line) {
        if (pn_get_block_parselet(parser) != pn_parse_comment) return PN_PARSER_STATE_NODE_EXIT;
        pn_eat_token(lex); pn_eat_one_whitespace(lex);
    } else {
        if (pn_peek_token_greedy(lex, 1)->type == PN_TOKEN_EOF) return PN_PARSER_STATE_NODE_EXIT;
        pn_push_text(parser, pn_eat_token(lex), PN_EVENT_TEXT_COMMENT);
    }

    return PN_PARSER_STATE_CURRENT;
}

static Pn_Parser_State pn_parse_list_item (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    (void)parser;
    Pn_Token *token = pn_eat_token(lex);
    switch (token->type) {
    case PN_TOKEN_NUMBER: frame->node->type = PN_AST_LIST_BY_NUMBER_ITEM; pn_eat_token(lex); break;
    case '-': frame->node->type = (token->txt.len == 2) ? PN_AST_LIST_ITEM : PN_AST_LIST_BY_BULLET_ITEM; break;
    case '[': frame->node->type = PN_AST_LIST_BY_CHECKBOX_ITEM;
              frame->node->as.checked_list_item.is_checked = (*pn_eat_token(lex)->txt.buf == 'x');
              if (pn_peek_token(lex, 1)->type == ']') pn_eat_token(lex);
              break;
    }

    pn_eat_one_whitespace(lex);
    frame->child_indentation++;
    return PN_PARSER_STATE_PARSING_BLOCKS;
}

static Pn_Parser_State pn_parse_list (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    if (frame->state == PN_PARSER_STATE_NODE_ENTER) {
        Pn_Token *token = pn_peek_token(lex, 1);
        frame->as.list.first_marker = token->type;

        switch (token->type) {
        case PN_TOKEN_NUMBER: frame->node->type = PN_AST_LIST_BY_NUMBER; break;
        case '-': frame->node->type = (token->txt.len == 2) ? PN_AST_LIST : PN_AST_LIST_BY_BULLET; break;
        case '[': frame->node->type = PN_AST_LIST_BY_CHECKBOX; break;
        default:  break;
        }

        return PN_PARSER_STATE_LIST_ITEMS;
    }

    Pn_Parselet P = pn_get_block_parselet(parser);
    if (P != pn_parse_list || parser->newest_node->type == PN_AST_PARAGRAPH_BLANK) return PN_PARSER_STATE_NODE_EXIT;
    if (frame->as.list.first_marker != pn_peek_token(lex, 1)->type) return PN_PARSER_STATE_NODE_EXIT;
    pn_push_frame(parser, pn_parse_list_item);
    return PN_PARSER_STATE_CURRENT;
}

static Pn_Parser_State pn_parse_meta_inline (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    if (frame->state == PN_PARSER_STATE_NODE_ENTER) {
        frame->node->type = PN_AST_META_INLINE;
        pn_eat_token(lex);
        frame->delim     = ']';
        frame->delim_len = 2;
        frame->node->as.meta.data.buf = parser->parsed_txt.buf ? frame->node->txt.buf : pn_peek_token(lex, 1)->txt.buf;
        frame->node->as.meta.data.len = 0;
        return PN_PARSER_STATE_META;
    }

    if (lex->at_start_of_line && pn_get_block_parselet(parser) != pn_parse_paragraph) return PN_PARSER_STATE_NODE_EXIT;
    Pn_Token *token = pn_eat_token(lex);
    if (token->type == '\\') { token = pn_eat_token(lex); token->type = PN_TOKEN_WORD; }
    if (token->type == PN_TOKEN_EOF || pn_match_token_eq(token, ']', 2)) return PN_PARSER_STATE_NODE_EXIT;
    if (token->type == ':') { pn_eat_one_whitespace(lex); parser->event.type = PN_EVENT_TEXT_META_DONE; return PN_PARSER_STATE_PARSING_INLINES; }

    pn_push_text(parser, token, PN_EVENT_TEXT_META);
    frame->node->as.meta.data.len += token->txt.len;
    return PN_PARSER_STATE_CURRENT;
}

static Pn_Parser_State pn_parse_meta_block (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    if (frame->state == PN_PARSER_STATE_NODE_ENTER) {
        frame->node->type = PN_AST_META_BLOCK;
        pn_eat_token(lex);
        frame->delim     = ']';
        frame->delim_len = 3;
        frame->node->as.meta.data.buf = parser->parsed_txt.buf ? frame->node->txt.buf : pn_peek_token(lex, 1)->txt.buf;
        frame->node->as.meta.data.len = 0;
        return PN_PARSER_STATE_META;
    }

    if (lex->at_start_of_line) {
        if (!pn_eat_indentation(lex, frame->child_indentation)) return PN_PARSER_STATE_NODE_EXIT;
        if (pn_match_token(pn_peek_token(lex, 1), ']', 3)) { pn_advance_to_next_line(lex); return PN_PARSER_STATE_NODE_EXIT; }
    }

    Pn_Token *token = pn_eat_token(lex);
    if (token->type == '\\') { token = pn_eat_token(lex); token->type = PN_TOKEN_WORD; }
    if (token->type == PN_TOKEN_EOF) return PN_PARSER_STATE_NODE_EXIT;
    if (token->type == ':') { pn_advance_to_next_line(lex); parser->event.type = PN_EVENT_TEXT_META_DONE; return PN_PARSER_STATE_PARSING_BLOCKS; }

    pn_push_text(parser, token, PN_EVENT_TEXT_META);
    frame->node->as.meta.data.len += token->txt.len;
    return PN_PARSER_STATE_CURRENT;
}

static Pn_Parser_State pn_parse_meta_tree (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    if (frame->state == PN_PARSER_STATE_NODE_ENTER) {
        frame->node->type = PN_AST_META_TREE;
        frame->child_indentation++;
        pn_eat_token(lex);
        frame->node->as.meta.data.buf = parser->parsed_txt.buf ? frame->node->txt.buf : pn_peek_token(lex, 1)->txt.buf;
        frame->node->as.meta.data.len = 0;
        return PN_PARSER_STATE_META;
    }

    Pn_Token *token = pn_eat_token(lex);
    if (token->type == '\\') { token = pn_eat_token(lex); token->type = PN_TOKEN_WORD; }
    if (token->type == PN_TOKEN_EOF) return PN_PARSER_STATE_NODE_EXIT;
    if (token->type == PN_TOKEN_NEWLINE) { parser->event.type = PN_EVENT_TEXT_META_DONE; return PN_PARSER_STATE_PARSING_BLOCKS; }
    if (token->type == ':') { pn_eat_one_whitespace(lex); parser->event.type = PN_EVENT_TEXT_META_DONE; return PN_PARSER_STATE_PARSING_BLOCKS; }

    pn_push_text(parser, token, PN_EVENT_TEXT_META);
    frame->node->as.meta.data.len += token->txt.len;
    return PN_PARSER_STATE_CURRENT;
}

static Pn_Parser_State pn_parse_format_block (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    switch (frame->state) {
    case PN_PARSER_STATE_NODE_ENTER: {
        frame->node->type = PN_AST_FORMAT_BLOCK;
        frame->node->as.format_block.info.buf = parser->parsed_txt.buf ? frame->node->txt.buf : pn_peek_token(lex, 1)->txt.buf;
        frame->node->as.format_block.info.len = 0;
        frame->delim     = '`';
        frame->delim_len = 3;

        pn_eat_token(lex);
        if (pn_peek_token(lex, 1)->type == '\\') {
            pn_eat_token(lex);
            frame->node->type = PN_AST_FORMAT_BLOCK_STRICT;
            parser->flags |= PN_PARSER_FLAG_STRICT_FORMAT_MODE;
        } else {
            parser->flags |= PN_PARSER_FLAG_FORMAT_MODE;
        }
    } return PN_PARSER_STATE_FORMAT_META;

    case PN_PARSER_STATE_FORMAT_META: {
        Pn_Token *token = pn_eat_token(lex);
        if (token->type == PN_TOKEN_EOF) { parser->flags &= ~(PN_PARSER_FLAG_FORMAT_MODE | PN_PARSER_FLAG_STRICT_FORMAT_MODE); return PN_PARSER_STATE_NODE_EXIT; }
        if (token->type == PN_TOKEN_NEWLINE) return PN_PARSER_STATE_PARSING_BLOCKS_AND_CALL_BEFORE_POP;
        pn_push_text(parser, token, PN_EVENT_TEXT_META);
        frame->node->as.format_block.info.len += token->txt.len;
    } break;

    case PN_PARSER_STATE_NODE_EXIT: parser->flags &= ~(PN_PARSER_FLAG_FORMAT_MODE | PN_PARSER_FLAG_STRICT_FORMAT_MODE); break;
    default: break;
    }

    return PN_PARSER_STATE_CURRENT;
}

static void table_skip_col (Pn_Lexer *lex, int table_indentation) {
    while (pn_advance_to_next_line(lex)) {
        if (!pn_eat_indentation(lex, table_indentation) && !pn_rest_of_line_is_empty(lex)) break;
        Pn_Token_Type T = pn_peek_token(lex, 1)->type;
        if (T != PN_TOKEN_WHITESPACE && T != PN_TOKEN_NEWLINE) break;
    }
}

static int table_count_cols (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *table_frame) {
    struct Pn_Table_Frame *T = &table_frame->as.table;
    if (T->cols_counted) return table_frame->node->as.table.n_cols;
    T->cols_counted = 1;

    Pn_Lexer backup = *lex;
    table_frame->node->as.table.n_cols = T->col;

    while (table_frame->node->as.table.n_cols < PN_MAX_TABLE_COLS) {
        table_skip_col(lex, table_frame->child_indentation);
        if (pn_peek_token(lex, 1)->type != '|' || pn_peek_token(lex, 2)->type == '-') break;
        table_frame->node->as.table.n_cols++;
    }

    parser->lexer = backup;
    return table_frame->node->as.table.n_cols;
}

static int table_count_rows (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *table_frame) {
    struct Pn_Table_Frame *T = &table_frame->as.table;
    if (table_frame->node->as.table.n_rows) return table_frame->node->as.table.n_rows;

    Pn_Lexer backup = *lex;
    table_frame->node->as.table.n_rows = T->row - 1;
    table_skip_col(lex, table_frame->child_indentation);

    Pn_Token_Type tok = PN_TOKEN_EOF;
    while (pn_peek_token(lex, 1)->type == '|') {
        tok = pn_peek_token(lex, 2)->type;
        if (tok == '-') table_frame->node->as.table.n_rows++;
        table_skip_col(lex, table_frame->child_indentation);
    }

    if (tok != '-') table_frame->node->as.table.n_rows++; // Check whether the table ended with a '|--' delim.
    parser->lexer = backup;
    return table_frame->node->as.table.n_rows;
}

static int table_cell_is_merged (struct Pn_Table_Frame *T) {
    return T->cell_merge_status && (T->row < T->cell_merge_status[T->col - 1]);
}

static void table_on_row_end (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    frame->as.table.row++; frame->as.table.col = 1;
    if (pn_eat_indentation(lex, frame->child_indentation) && pn_peek_token(lex, 1)->type == '|') parser->event.type = PN_EVENT_TABLE_ROW_END;
}

static Pn_Parser_State pn_parse_table_cell (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    Pn_Parselet_Frame *table_frame = frame - 1;
    struct Pn_Table_Frame *T       = &table_frame->as.table;
    Pn_Ast *N                      = frame->node;
    N->type                        = PN_AST_TABLE_CELL;
    N->as.table_cell.col           = T->col;
    N->as.table_cell.row           = T->row;
    N->as.table_cell.width         = 1;
    N->as.table_cell.height        = 1;

    if (table_frame->state == PN_PARSER_STATE_TABLE_ADDING_MISSING_CELLS) { T->col++; return PN_PARSER_STATE_NODE_EXIT; }

    { // Parse cell dimensions if any.
        int n_cols = 0;
        Pn_Token *token = pn_peek_token(lex, 1);

        { // width
            if (token->type != PN_TOKEN_NUMBER && !pn_match_token_eq(token, '*', 1)) goto DONE;
            n_cols  = table_count_cols(parser, lex, table_frame);
            int max = n_cols - T->col + 1;
            N->as.table_cell.width = (token->type == '*') ? max : pn_parse_num(token, 1, max);
            pn_eat_token(lex);
        }

        if (! pn_match_token_eq(pn_peek_token(lex, 1), '|', 1)) goto DONE;
        pn_eat_token(lex); token = pn_peek_token(lex, 1);

        { // height
            if (token->type != PN_TOKEN_NUMBER && !pn_match_token_eq(token, '*', 1)) goto DONE;
            int n_rows = table_count_rows(parser, lex, table_frame);
            int max    = n_rows - T->row + 1;
            N->as.table_cell.height = (token->type == '*') ? max : pn_parse_num(token, 1, max);
            pn_eat_token(lex);
        }

DONE:   if (N->as.table_cell.height != 1 || N->as.table_cell.width != 1) {
            if (! T->cell_merge_status) T->cell_merge_status = parser->alloc.calloc((size_t)n_cols, sizeof(int));
            if (! T->cell_merge_status) return PN_PARSER_STATE_ALLOC_FAIL;

            int h = T->row + N->as.table_cell.height;
            int w = N->as.table_cell.width;

            for (int i = T->col-1; (i < n_cols) && w; ++i, --w) {
                if (T->row < T->cell_merge_status[i]) { N->as.table_cell.width = i - T->col + 1; break; }
                T->cell_merge_status[i] = h;
            }
        }
    }

    if (pn_match_token_eq(pn_peek_token(lex, 1), ']', 1)) pn_eat_token(lex);
    pn_eat_one_whitespace(lex);
    T->col++;
    frame->child_indentation++;
    return PN_PARSER_STATE_PARSING_BLOCKS;
}

static Pn_Parser_State pn_parse_table (Pn_Parser parser, Pn_Lexer *lex, Pn_Parselet_Frame *frame) {
    struct Pn_Table_Frame *T = &frame->as.table;

    switch (frame->state) {
    case PN_PARSER_STATE_NODE_ENTER: {
        frame->node->type = PN_AST_TABLE;
        frame->node->as.table.n_rows = 0;
        frame->node->as.table.n_cols = PN_MAX_TABLE_COLS;
        pn_advance_to_next_line(lex);
        T->row               = 1;
        T->col               = 1;
        T->cols_counted      = 0;
        T->cell_merge_status = NULL;
    } return PN_PARSER_STATE_TABLE;

    case PN_PARSER_STATE_TABLE: {
        if (!pn_eat_indentation(lex, frame->child_indentation) || pn_peek_token(lex, 1)->type != '|') return PN_PARSER_STATE_TABLE_DONE;
        pn_eat_token(lex);

        if (pn_peek_token(lex, 1)->type == '-') { // End of row.
            pn_advance_to_next_line(lex);

            if (T->row == 1 && frame->node->as.table.n_cols == PN_MAX_TABLE_COLS) {
                T->cols_counted = 1;
                frame->node->as.table.n_cols = T->col - 1;
            } else if (T->col <= frame->node->as.table.n_cols) {
                return PN_PARSER_STATE_TABLE_ADDING_MISSING_CELLS;
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
            pn_push_frame(parser, pn_parse_table_cell);
        }
    } break;

    case PN_PARSER_STATE_TABLE_DONE: {
        if (! T->cols_counted) frame->node->as.table.n_cols = T->col - 1;

        if      (T->col == 1) T->row--;
        else if (T->col <= frame->node->as.table.n_cols) return PN_PARSER_STATE_TABLE_ADDING_MISSING_CELLS;

        frame->node->as.table.n_rows = T->row;
        parser->alloc.free(T->cell_merge_status);
        T->cell_merge_status = NULL;
    } return PN_PARSER_STATE_NODE_EXIT;

    case PN_PARSER_STATE_TABLE_ADDING_MISSING_CELLS: {
        if (T->col > frame->node->as.table.n_cols) { table_on_row_end(parser, lex, frame); return PN_PARSER_STATE_TABLE; }
        else if (table_cell_is_merged(T)) T->col++;
        else pn_push_frame(parser, pn_parse_table_cell);
    } break;

    default: break;
    }

    return PN_PARSER_STATE_CURRENT;
}

static Pn_Parselet pn_get_block_parselet (Pn_Parser parser) {
    Pn_Lexer *lex = &parser->lexer;

    if (pn_peek_token(lex, 1)->type == PN_TOKEN_EOF) return NULL;
    if (parser->frames_count >= PN_MAX_RECURSION_DEPTH) { pn_eat_token(lex); return pn_parse_paragraph; } // pn_eat_token() prevents inf loops.
    if (pn_rest_of_line_is_empty(lex)) return pn_parse_paragraph_blank;
    if (lex->at_start_of_line && !pn_eat_indentation(lex, parser->frames[parser->frames_count - 1].child_indentation)) return NULL;

    int S = parser->flags & PN_PARSER_FLAG_STRICT_FORMAT_MODE;
    int F = S || parser->flags & PN_PARSER_FLAG_FORMAT_MODE;

    Pn_Token *token = pn_peek_token(lex, 1);
    switch (token->type) {
    case PN_TOKEN_EOF:    return NULL;
    case PN_TOKEN_NUMBER: return !F && (pn_peek_token(lex, 2)->type == '.') ? pn_parse_list : pn_parse_paragraph;

    case '#': return !F                        ? pn_parse_header          : pn_parse_paragraph;
    case '`': return       token->txt.len >= 3 ? pn_parse_format_block    : pn_parse_paragraph;
    case '|': return !F && token->txt.len == 1 ? pn_parse_table           : pn_parse_paragraph;
    case '/': return !F && token->txt.len >= 2 ? pn_parse_comment         : pn_parse_paragraph;
    case '=': return !F && token->txt.len >= 3 ? pn_parse_separator       : pn_parse_paragraph;
    case '>': return !F && token->txt.len <= 2 ? pn_parse_blockquote      : pn_parse_paragraph;
    case '-': return !F && token->txt.len <= 2 ? pn_parse_list            : pn_parse_paragraph;
    case ':': return !S && token->txt.len == 1 ? pn_parse_meta_tree       : pn_parse_paragraph;
    case ']': return !S && token->txt.len >= 3 ? pn_parse_paragraph_blank : pn_parse_paragraph;

    case '[': {
        if (!S && token->txt.len >= 3) return pn_parse_meta_block;

        if (!F && token->txt.len == 1) {
            Pn_Token *T2 = pn_peek_token(lex, 2), *T3 = pn_peek_token(lex, 3);
            if (token->txt.len != 1 || T2->txt.len != 1) break;
            if (T3->type == ']' && (*T2->txt.buf == ' ' || *T2->txt.buf == 'x')) return pn_parse_list;
        }
    } break;
    }

    return pn_parse_paragraph;
}

static Pn_Parselet pn_get_inline_parselet (Pn_Parser parser) {
    Pn_Lexer *lex = &parser->lexer;
    int S         = parser->flags & PN_PARSER_FLAG_STRICT_FORMAT_MODE;
    int F         = S || parser->flags & PN_PARSER_FLAG_FORMAT_MODE;

    if (lex->at_start_of_line && pn_get_block_parselet(parser) != pn_parse_paragraph) return NULL;

    Pn_Token *token = F ? pn_peek_token(lex, 1) : pn_peek_token_greedy(lex, 1);
    if (token->type == PN_TOKEN_EOF) return NULL;
    if (parser->frames_count >= PN_MAX_RECURSION_DEPTH) return pn_parse_text;

    switch (token->type) {
    case '\\': if (!S) { pn_eat_token(lex); return pn_parse_text; } break;
    case '[':  return !S && token->txt.len == 2 ? pn_parse_meta_inline    : pn_parse_text;
    case '*':  return !F                        ? pn_parse_emphasis       : pn_parse_text;
    case '^':  return !F                        ? pn_parse_sub_sup_script : pn_parse_text;
    case '`':  return !F                        ? pn_parse_format_inline  : pn_parse_text;
    case '<':  return !F && token->txt.len >= 2 ? pn_parse_link           : pn_parse_text;
    case '~':  return !F && token->txt.len == 2 ? pn_parse_strikethrough  : pn_parse_text;
    case '|':  return !F && pn_peek_token(lex, 2)->type == PN_TOKEN_NEWLINE ? pn_parse_line_break : pn_parse_text;
    }

    return pn_parse_text;
}

static int pn_make_node_pool (Pn_Parser parser) {
    Pn_Ast_Pool *pool = parser->alloc.malloc(sizeof(*pool));
    if (! pool) return 0;

    pool->nodes      = parser->alloc.calloc((size_t)parser->ast_pool_capacity, sizeof(Pn_Ast));
    pool->next       = parser->ast_pool;
    parser->ast_pool = pool;
    parser->ast_pool_item_count = 0;
    return (pool->nodes != NULL);
}

static Pn_Ast *pn_make_node (Pn_Parser parser) {
    if (parser->ast_pool_item_count == parser->ast_pool_capacity && !pn_make_node_pool(parser)) return NULL;

    Pn_Token *token     = pn_peek_token(&parser->lexer, 1);
    Pn_Ast *node        = &parser->ast_pool->nodes[parser->ast_pool_item_count++];
    node->txt.buf       = parser->parsed_txt.buf ? (parser->parsed_txt.buf + parser->parsed_txt.len) : token->txt.buf;
    node->start_line    = token->line;
    node->first_child   = NULL;
    node->right_sibling = NULL;
    parser->newest_node = node;
    return node;
}

static void pn_push_frame (Pn_Parser parser, Pn_Parselet parselet) {
    assert(parser->frames_count <= PN_MAX_RECURSION_DEPTH);

    Pn_Parselet_Frame *frame        = &parser->frames[parser->frames_count++];
    Pn_Parselet_Frame *parent_frame = frame - 1;
    assert(parent_frame->state != PN_PARSER_STATE_NODE_ENTER);

    Pn_Ast *node = pn_make_node(parser);
    if (! node) { frame->state = PN_PARSER_STATE_ALLOC_FAIL; return; }

    *parent_frame->node_attach_point = node;
    parent_frame->node_attach_point  = &node->right_sibling;

    frame->node              = node;
    frame->parselet          = parselet;
    frame->node_attach_point = &node->first_child;
    frame->delim             = PN_TOKEN_EOF;
    frame->child_indentation = parent_frame->child_indentation;
    frame->state             = PN_PARSER_STATE_NODE_ENTER;
    frame->state             = parselet(parser, &parser->lexer, frame);

    parser->event.type    = PN_EVENT_NODE_ENTER;
    parser->event.as.node = node;
}

static void pn_pop_frame (Pn_Parser parser) {
    if (parser->frames_count == 1) {
        parser->event.type = PN_EVENT_EOF;
        parser->frames[0].state = PN_PARSER_STATE_EOF;
        return;
    }

    Pn_Parselet_Frame *frame = &parser->frames[--parser->frames_count];

    if (frame->state == PN_PARSER_STATE_PARSING_BLOCKS_AND_CALL_BEFORE_POP) {
        frame->state = PN_PARSER_STATE_NODE_EXIT;
        frame->parselet(parser, &parser->lexer, frame);
        if (frame->state == PN_PARSER_STATE_ALLOC_FAIL) return;
    }

    frame->node->txt.len  = parser->parsed_txt.buf ? (int)(parser->parsed_txt.buf + parser->parsed_txt.len - frame->node->txt.buf) : 0;
    frame->node->end_line = pn_peek_token(&parser->lexer, 1)->line;
    if (parser->flags & PN_PARSER_FLAG_PREV_PARSED_IS_NEWLINE) frame->node->end_line--;
    if (parser->flags & PN_PARSER_FLAG_NO_AST) parser->ast_pool_item_count--;

    parser->event.type    = PN_EVENT_NODE_EXIT;
    parser->event.as.node = frame->node;
}


// =============================================================================
// @@@ Implementation / Parser / Functions / Public
// =============================================================================
Pn_Parser pn_new_with_alloc (char *buf, int buf_len, int make_ast, Pn_Allocators alloc) {
    Pn_Parser parser = alloc.calloc(1, sizeof(*parser));
    if (! parser) return NULL;

    if (make_ast) {
        parser->ast_pool_capacity = (buf_len < 800) ? 8 : (buf_len / 100);
        parser->parsed_txt.buf    = alloc.malloc((size_t)buf_len);
        if (! parser->parsed_txt.buf) { alloc.free(parser); return NULL; }
    } else {
        parser->flags |= PN_PARSER_FLAG_NO_AST;
        parser->ast_pool_capacity = PN_MAX_RECURSION_DEPTH + 1; // The one pool will be used as a stack.
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
        if (! pn_make_node_pool(parser)) { pn_free(parser); return NULL; }

        Pn_Parselet_Frame *frame = &parser->frames[parser->frames_count++];
        frame->node = pn_make_node(parser);
        if (! frame->node) { pn_free(parser); return NULL; }

        frame->state             = PN_PARSER_STATE_PARSING_BLOCKS;
        parser->root             = frame->node;
        frame->node->type        = PN_AST_ROOT;
        frame->node_attach_point = &frame->node->first_child;
    }

    return parser;
}

Pn_Parser pn_new (char *buf, int buf_len, int make_ast) {
    Pn_Allocators alloc = { malloc, calloc, free };
    return pn_new_with_alloc(buf, buf_len, make_ast, alloc);
}

Pn_Ast *pn_get_root (Pn_Parser parser) {
    return parser->root;
}

void pn_free (Pn_Parser parser) {
    Pn_Ast_Pool *pool = parser->ast_pool;
    while (pool) {
        Pn_Ast_Pool *tmp = pool->next;
        parser->alloc.free(pool->nodes);
        parser->alloc.free(pool);
        pool = tmp;
    }

    // If needed, we can make a free list for all heap allocs to make this cleaner.
    for (int i = 0 ; i < parser->frames_count; ++i) {
        Pn_Parselet_Frame *frame = &parser->frames[i];
        if (frame->parselet == pn_parse_table) parser->alloc.free(frame->as.table.cell_merge_status);
    }

    parser->alloc.free(parser->parsed_txt.buf);
    parser->alloc.free(parser);
}

Pn_Event *pn_next (Pn_Parser parser) {
    assert(parser->frames_count > 0);

    Pn_Lexer *lex            = &parser->lexer;
    Pn_Parselet_Frame *frame = &parser->frames[parser->frames_count - 1];
    parser->event.type       = PN_EVENT_NONE;

REPEAT_SWITCH:
    switch (frame->state) {
    case PN_PARSER_STATE_EOF:       parser->event.type = PN_EVENT_EOF; break;
    case PN_PARSER_STATE_NODE_EXIT: pn_pop_frame(parser); break;

    case PN_PARSER_STATE_PARSING_BLOCKS:
    case PN_PARSER_STATE_PARSING_BLOCKS_AND_CALL_BEFORE_POP: {
        Pn_Parselet P = pn_get_block_parselet(parser);
        if (! P) { pn_pop_frame(parser); }
        else if (pn_match_token(pn_peek_token(lex, 1), frame->delim, frame->delim_len)) { pn_pop_frame(parser); pn_advance_to_next_line(lex); }
        else { pn_push_frame(parser, P); }
    } break;

    case PN_PARSER_STATE_PARSING_INLINES: {
        Pn_Parselet P = pn_get_inline_parselet(parser);
        if (! P) pn_pop_frame(parser);
        else if (pn_match_token_eq(pn_peek_token(lex, 1), frame->delim, frame->delim_len)) { pn_pop_frame(parser); pn_eat_token(lex); }
        else if (P == pn_parse_text) P(parser, &parser->lexer, frame);
        else pn_push_frame(parser, P);
    } break;

    default: {
        Pn_Parser_State S = frame->parselet(parser, lex, frame);
        int parselet_has_pushed_frame = (frame != &parser->frames[parser->frames_count - 1]);
        if (S == PN_PARSER_STATE_CURRENT || parselet_has_pushed_frame) break;
        else if (S == PN_PARSER_STATE_NODE_EXIT) pn_pop_frame(parser);
        else frame->state = S;

        if (parser->event.type == PN_EVENT_NONE) goto REPEAT_SWITCH;
    } break;
    }

    if (frame->state == PN_PARSER_STATE_ALLOC_FAIL) parser->event.type = PN_EVENT_ALLOC_FAIL;
    parser->event.indentation = frame->child_indentation;
    return &parser->event;
}

#endif // PNAKOTIC_IMPLEMENTATION


// =============================================================================
// @@@ License
//
// Choose one of the following:
//
// (License 1)
// MIT License Copyright (c) 2019 zagortenay333
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions: The above copyright
// notice and this permission notice shall be included in all copies or
// substantial portions of the Software.  THE SOFTWARE IS PROVIDED "AS IS",
// WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
// FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
// THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// (License 2)
// Public Domain (www.unlicense.org)
// This is free and unencumbered software released into the public domain.
// Anyone is free to copy, modify, publish, use, compile, sell, or distribute
// this software, either in source code form or as a compiled binary, for any
// purpose, commercial or non-commercial, and by any means.  In jurisdictions
// that recognize copyright laws, the author or authors of this software
// dedicate any and all copyright interest in the software to the public domain.
// We make this dedication for the benefit of the public at large and to the
// detriment of our heirs and successors. We intend this dedication to be an
// overt act of relinquishment in perpetuity of all present and future rights to
// this software under copyright law.  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT
// WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
// WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.
// =============================================================================
