#include <stdio.h>
#include <stdlib.h>

#include <sanitizer/lsan_interface.h>

#define MARKUP_IMPLEMENTATION
#include "../markup_parser.h"


// =============================================================================
// @@@ Data
// =============================================================================
typedef struct String_Fragment {
    char  *buf;
    size_t buf_payload;
    struct String_Fragment *next;
} String_Fragment;

typedef struct {
    M_Parser parser;
    char num_scratch_buf[9];
    size_t fragment_capacity;
    String_Fragment *first_fragment, *last_fragment;
} Emitter;

static char *type_to_str[] = {
    "M_AST_ROOT",
    "M_AST_PARAGRAPH",
    "M_AST_PARAGRAPH_BLANK",
    "M_AST_TABLE",
    "M_AST_TABLE_CELL",
    "M_AST_LIST",
    "M_AST_LIST_ITEM",
    "M_AST_LIST_BY_BULLET",
    "M_AST_LIST_BY_BULLET_ITEM",
    "M_AST_LIST_BY_NUMBER",
    "M_AST_LIST_BY_NUMBER_ITEM",
    "M_AST_LIST_BY_CHECKBOX",
    "M_AST_LIST_BY_CHECKBOX_ITEM",
    "M_AST_FORMAT_BLOCK",
    "M_AST_FORMAT_BLOCK_STRICT",
    "M_AST_FORMAT_INLINE",
    "M_AST_META_TREE",
    "M_AST_META_BLOCK",
    "M_AST_META_INLINE",
    "M_AST_HEADER",
    "M_AST_COMMENT",
    "M_AST_SEPARATOR",
    "M_AST_BLOCKQUOTE",
    "M_AST_LINK",
    "M_AST_EMPHASIS",
    "M_AST_LINE_BREAK",
    "M_AST_SUP_SCRIPT",
    "M_AST_SUB_SCRIPT",
    "M_AST_STRIKETHROUGH"
};

#define NORMAL "\x1B[0;m"
#define RED    "\x1B[1;38;5;1m"
#define GREEN  "\x1B[1;38;5;2m"


// =============================================================================
// @@@ Functions
// =============================================================================
char *read_entire_file (char *filename, size_t *out_size) {
    long file_size;
    size_t n_read;
    char *buffer;

    FILE *f = fopen(filename, "rb");
    if (! f) return NULL;

    fseek(f, 0, SEEK_END);
    file_size = ftell(f);
    if (file_size < 0) { fclose(f); return NULL; }
    rewind(f);

    buffer = malloc((size_t)file_size + 1);
    if (! buffer) return NULL;

    n_read = fread(buffer, 1, (size_t)file_size, f);
    if (n_read != (size_t)file_size) { free(buffer); return NULL; }

    buffer[(size_t)file_size] = '\0';
    if (file_size > 1) buffer[(size_t)file_size - 1] = '\n';

    fclose(f);
    if (out_size) *out_size = (size_t)file_size;
    return buffer;
}

int write_entire_file (char *filename, char *buf, size_t size) {
    size_t n_written;
    FILE *f = fopen(filename, "wb");
    if (! f) return 0;
    n_written = fwrite(buf, 1, size, f);
    fclose(f);
    return n_written == size;
}

static char *emitter_finalize (Emitter *emitter) {
    size_t buf_len = 0;
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

    m_free(emitter->parser);
    if (buf) buf[buf_len] = '\0';
    return buf;
}

static void emitter_make_fragment (Emitter *emitter) {
    String_Fragment *tmp = calloc(1, sizeof(*tmp));
    tmp->buf                     = calloc(1, emitter->fragment_capacity);
    emitter->last_fragment->next = tmp;
    emitter->last_fragment       = tmp;
}

static void emitter_write_to_fragment (Emitter *emitter, char *str, size_t str_len) {
    char *top = emitter->last_fragment->buf + emitter->last_fragment->buf_payload;
    memcpy(top, str, str_len);
    emitter->last_fragment->buf_payload += str_len;
}

static void emitter_append_n (Emitter *emitter, char *str, size_t str_len) {
    if (str_len == 0) return;

    while (1) {
        size_t available = emitter->fragment_capacity - emitter->last_fragment->buf_payload;

        if (str_len <= available) {
            emitter_write_to_fragment(emitter, str, str_len);
            break;
        } else {
            emitter_write_to_fragment(emitter, str, available);
            emitter_make_fragment(emitter);
            str     += available;
            str_len -= available;
        }
    }
}

static void emitter_append (Emitter *emitter, char *buf) {
    emitter_append_n(emitter, buf, strlen(buf));
}

static void emitter_append_num (Emitter *emitter, int num) {
    if (num == 0) {
        emitter_append_n(emitter, "0", 1);
        return;
    }

    char *cursor = emitter->num_scratch_buf;
    char *end    = cursor + 9;

    while (cursor != end) {
        int tmp = num;
        *cursor++ = "0123456789"[tmp - (num /= 10) * 10];
    }

    char ch;
    char *left_cursor  = emitter->num_scratch_buf;
    char *right_cursor = cursor - 1;

    while (right_cursor > left_cursor) {
        ch            = *left_cursor;
        *left_cursor  = *right_cursor;
        *right_cursor = ch;
        left_cursor++;
        right_cursor--;
    }

    size_t n = 9;
    cursor = emitter->num_scratch_buf;
    while (*cursor == '0') { n--; cursor++; }
    emitter_append_n(emitter, cursor, n);
}

static void emitter_mainloop (Emitter *emitter) {
    while (1) {
        M_Event *e = m_next(emitter->parser);
        switch (e->type) {
        case M_EVENT_EOF:        return;
        case M_EVENT_ALLOC_FAIL: return;

        case M_EVENT_NODE_ENTER: {
            emitter_append(emitter, "i ");
            emitter_append_num(emitter, e->indentation);
            emitter_append(emitter, " ");
            emitter_append_num(emitter, e->as.node->start_line);
            emitter_append(emitter, " ");
            emitter_append(emitter, type_to_str[e->as.node->type]);
            emitter_append(emitter, "\n");
        } break;

        case M_EVENT_NODE_EXIT: {
            emitter_append(emitter, "o ");
            emitter_append_num(emitter, e->indentation);
            emitter_append(emitter, " ");
            emitter_append_num(emitter, e->as.node->end_line);
            emitter_append(emitter, " ");
            emitter_append(emitter, type_to_str[e->as.node->type]);

            switch (e->as.node->type) {
            case M_AST_TABLE: {
                emitter_append(emitter, " ");
                emitter_append_num(emitter, e->as.node->as.table.n_rows);
                emitter_append(emitter, " ");
                emitter_append_num(emitter, e->as.node->as.table.n_cols);
            } break;

            case M_AST_TABLE_CELL: {
                emitter_append(emitter, " ");
                emitter_append_num(emitter, e->as.node->as.table_cell.row);
                emitter_append(emitter, " ");
                emitter_append_num(emitter, e->as.node->as.table_cell.col);
                emitter_append(emitter, " ");
                emitter_append_num(emitter, e->as.node->as.table_cell.width);
                emitter_append(emitter, " ");
                emitter_append_num(emitter, e->as.node->as.table_cell.height);
            } break;

            default: break;
            }

            emitter_append(emitter, "\n");
        } break;

        default: break;
        }
    }
}

char *serialize_ast (char *in, size_t in_len) {
    Emitter emitter             = {0};
    emitter.fragment_capacity   = in_len;
    emitter.first_fragment      = calloc(1, sizeof(String_Fragment));
    emitter.last_fragment       = emitter.first_fragment;
    emitter.first_fragment->buf = malloc(in_len);
    emitter.parser              = m_new(in, (int)in_len, 0);
    emitter_mainloop(&emitter);
    return emitter_finalize(&emitter);
}

int do_test (char *in, char *out, char *in_file_path, char *out_file_path) {
    char *out_base = strrchr(out_file_path, '/');
    out_base ? ++out_base : (out_base = out_file_path);

    size_t line      = 1;
    size_t column    = 1;
    char *in_cursor  = in;
    char *out_cursor = out;

    while (*in_cursor && *out_cursor) {
        if (*in_cursor != *out_cursor) goto ON_ERR;
        column++;
        if (*in_cursor == '\n') { line++; column = 1; }
        ++in_cursor;
        ++out_cursor;
    }

    if (strlen(in) != strlen(out)) goto ON_ERR;

    printf("[" GREEN "PASSED" NORMAL "] %s\n" NORMAL, out_base);
    return EXIT_SUCCESS;

ON_ERR:
    printf("[" RED "FAILED" NORMAL "] %s line [%li] column [%li]\n", out_base, line, column);
    return EXIT_FAILURE;
}

int main (int argc, char **argv) {
    if (argc != 4) goto ON_ERR;

    int action = 0;
    if      (! strcmp(argv[1], "--serialize")) action = 1;
    else if (! strcmp(argv[1], "--test"))      action = 2;
    else                                       goto ON_ERR;

    __lsan_do_leak_check();

    size_t in_len = 0;
    char *in = read_entire_file(argv[2], &in_len);
    char *ser = serialize_ast(in, in_len);

    if (action == 1) write_entire_file(argv[3], ser, strlen(ser));
    else             do_test(ser, read_entire_file(argv[3], NULL), argv[2], argv[3]);

    free(in);
    free(ser);

    __lsan_disable(); // This prevents false positives on exit.

    return EXIT_SUCCESS;

ON_ERR:
    printf("Usage: %s (--serialize | --test) markup_file serialized_ast_file\n", argv[0]);
    return EXIT_FAILURE;
}
