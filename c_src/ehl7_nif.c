/**
* \file ehl7_nif.c
*
* Erlang NIFs for the HL7 parser library.
*
* \internal
* Copyright (c) 2003-2011 \b Erlar (http://erlar.com)
*/

/* ------------------------------------------------------------------------
   Headers
   ------------------------------------------------------------------------ */

#include <hl7parser/buffer.h>
#include <hl7parser/defs.h>
#include <hl7parser/element.h>
#include <hl7parser/settings.h>
#include <hl7parser/token.h>
#include <hl7parser/cbparser.h>
#include <erl_nif.h>
#include <alloca.h>
#include <string.h>
#include <stdio.h>


/* ------------------------------------------------------------------------
   Defines
   ------------------------------------------------------------------------ */

#define MAX_ELEMENT_DEPTH  256
#define BUFFER_SIZE        (8 * 1024)

#ifdef __GNUC__
#define UNUSED __attribute__ ((__unused__))
#endif
#ifndef NDEBUG
#define DEBUG(fmt)  fprintf(state->log, fmt)
#define DEBUG1(fmt, a1)  fprintf(state->log, fmt, a1)
#define DEBUG2(fmt, a1, a2)  fprintf(state->log, fmt, a1, a2)
#define DEBUG3(fmt, a1, a2, a3)  fprintf(state->log, fmt, a1, a2, a3)
#define DEBUG4(fmt, a1, a2, a3, a4)  fprintf(state->log, fmt, a1, a2, a3, a4)
static char *element_type_name(const HL7_Element_Type element_type);
#else
#define DEBUG(fmt)
#define DEBUG1(fmt, a1)
#define DEBUG2(fmt, a1, a2)
#define DEBUG3(fmt, a1, a2, a3)
#define DEBUG4(fmt, a1, a2, a3, a4)
#endif

typedef struct Decode_State_Struct {
    ErlNifEnv *env;
    ERL_NIF_TERM undefined_atom;
    ERL_NIF_TERM term[HL7_ELEMENT_TYPE_COUNT + 1][MAX_ELEMENT_DEPTH];
    int term_count[HL7_ELEMENT_TYPE_COUNT + 1];
#ifndef NDEBUG
    FILE *log;
#endif
} Decode_State;

typedef struct Encode_State_Struct {
    ErlNifEnv *env;
    ERL_NIF_TERM undefined_atom;
    ERL_NIF_TERM term[HL7_ELEMENT_TYPE_COUNT + 1][MAX_ELEMENT_DEPTH];
    int term_count[HL7_ELEMENT_TYPE_COUNT + 1];
#ifndef NDEBUG
    FILE *log;
#endif
} Encode_State;


static int load_nif(UNUSED ErlNifEnv* env, UNUSED void** priv_data, UNUSED ERL_NIF_TERM load_info);
static int reload_nif(UNUSED ErlNifEnv* env, UNUSED void** priv_data, UNUSED ERL_NIF_TERM load_info);
static int upgrade_nif(UNUSED ErlNifEnv* env, UNUSED void** priv_data, UNUSED void** old_priv_data, UNUSED ERL_NIF_TERM load_info);
static void unload_nif(UNUSED ErlNifEnv* env, UNUSED void* priv_data);

static ERL_NIF_TERM raw_decode(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv);
static ERL_NIF_TERM raw_encode(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv);

static int start_document(HL7_Parser *parser);
static int end_document(HL7_Parser *parser);
static int start_element(HL7_Parser *parser, HL7_Element_Type element_type);
static int end_element(HL7_Parser *parser, HL7_Element_Type element_type);
static int characters(HL7_Parser *parser, HL7_Element_Type element_type, HL7_Element *element);

static int encode_segment(Encode_State *state, HL7_Parser *parser, HL7_Allocator *allocator,
                          HL7_Buffer *buffer, ERL_NIF_TERM parent);
static int encode_tuple(Encode_State *state, HL7_Segment *segment, HL7_Element_Type element_type, size_t *index,
                        int tuple_size, const ERL_NIF_TERM *tuple);
static int set_element(HL7_Segment *segment, HL7_Element_Type element_type, size_t *index, HL7_Element *element);

static ERL_NIF_TERM nif_error(ErlNifEnv *env, const char *reason);
static ERL_NIF_TERM nif_error_data(ErlNifEnv *env, const char *reason, ERL_NIF_TERM data);
static int parent(HL7_Element_Type element_type);


static ErlNifFunc nif_funcs[] = {
    {"raw_decode", 1, raw_decode},
    {"raw_encode", 1, raw_encode}
};

ERL_NIF_INIT(ehl7, nif_funcs, load_nif, reload_nif, upgrade_nif, unload_nif)


/* ------------------------------------------------------------------------ */
static int load_nif(UNUSED ErlNifEnv* env, UNUSED void** priv_data, UNUSED ERL_NIF_TERM load_info)
{
    return 0;
}


/* ------------------------------------------------------------------------ */
static int reload_nif(UNUSED ErlNifEnv* env, UNUSED void** priv_data, UNUSED ERL_NIF_TERM load_info)
{
    return 0;
}


/* ------------------------------------------------------------------------ */
static int upgrade_nif(UNUSED ErlNifEnv* env, UNUSED void** priv_data, UNUSED void** old_priv_data, UNUSED ERL_NIF_TERM load_info)
{
    return 0;
}


/* ------------------------------------------------------------------------ */
static void unload_nif(UNUSED ErlNifEnv* env, UNUSED void* priv_data)
{
}


/* ------------------------------------------------------------------------ */
static ERL_NIF_TERM raw_decode(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv)
{
    ERL_NIF_TERM        result;
    ErlNifBinary        source;
    HL7_Settings        settings;
    HL7_Buffer          buffer;
    HL7_Parser          parser;
    HL7_Parser_Callback callback;
    Decode_State        state;

    if (argc != 1 || !enif_inspect_binary(env, argv[0], &source))
    {
        return enif_make_badarg(env);
    }

    hl7_settings_init(&settings);

    /* Initialize the buffer indicating the start and end of the binary. */
    hl7_buffer_init(&buffer, (char *) source.data, source.size);
    hl7_buffer_set_wr_ptr(&buffer, (char *) source.data + source.size);

    /* Initialize the callback parser. */
    hl7_parser_cb_init(&parser, &callback, &settings);
    memset(&state, 0, sizeof (state));
    state.env = env;
    state.undefined_atom = enif_make_atom(env, "undefined");
    hl7_parser_set_user_data(&parser, &state);

    /* Set the parser callbacks. */
    callback.start_document = start_document;
    callback.end_document   = end_document;
    callback.start_element  = start_element;
    callback.end_element    = end_element;
    callback.characters     = characters;

    /* Parse the contents of the buffer with the callback parser. */
    if (hl7_parser_cb_read(&parser, &callback, &buffer) == 0)
    {
        result = enif_make_list_from_array(env, &state.term[HL7_ELEMENT_SEGMENT + 1][0], state.term_count[HL7_ELEMENT_SEGMENT + 1]);
    }
    else
    {
        result = enif_make_badarg(env);
    }

    hl7_parser_cb_fini(&parser);
    hl7_buffer_fini(&buffer);
    hl7_settings_fini(&settings);

    return result;
}


/* ------------------------------------------------------------------------ */
ERL_NIF_TERM raw_encode(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv)
{
    ERL_NIF_TERM        result;
    ErlNifBinary        bin;
    HL7_Settings        settings;
    HL7_Allocator       allocator;
    HL7_Buffer          buffer;
    HL7_Parser          parser;
    ERL_NIF_TERM        list, head, tail;
    Encode_State        state;

    if (argc != 1 || !enif_is_list(env, argv[0]))
    {
        return enif_make_badarg(env);
    }
    if (!enif_alloc_binary(BUFFER_SIZE, &bin))
    {
        return nif_error(env, "enomem");
    }

    hl7_settings_init(&settings);
    /* Initialize the memory allocator. */
    hl7_allocator_init(&allocator, malloc, free);
    /* Initialize the output buffer. */
    hl7_buffer_init(&buffer, (char *) bin.data, bin.size);
    /* Initialize the parser. */
    hl7_parser_init(&parser, &settings);

    memset(&state, 0, sizeof (state));
    state.env = env;

#ifndef NDEBUG
    state.log = fopen("ehl7_encode.log", "a");
    if (state.log == NULL)
    {
        fprintf(stderr, "Could not open log file 'ehl7_encode.log'\n");
    }
#endif

    for (list = argv[0]; enif_get_list_cell(env, list, &head, &tail); list = tail)
    {
        if (encode_segment(&state, &parser, &allocator, &buffer, head) != 0)
        {
            result = nif_error_data(env, "invalid_segment", head);
            goto error;
        }
    }

    if (enif_realloc_binary(&bin, hl7_buffer_length(&buffer)))
    {
        result = enif_make_binary(env, &bin);
    }
    else
    {
        enif_release_binary(&bin);
        result = nif_error(env, "enomem");
    }

error:
#ifndef NDEBUG
    if (state.log != NULL)
    {
        fclose(state.log);
    }
    state.log = NULL;
#endif

    hl7_parser_fini(&parser);
    hl7_buffer_fini(&buffer);
    hl7_allocator_fini(&allocator);
    hl7_settings_fini(&settings);

    return result;
}


/* ------------------------------------------------------------------------ */
/* Internal functions                                                       */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
static int start_document(UNUSED HL7_Parser *parser)
{
    int rc = 0;

#ifndef NDEBUG
    Decode_State *state;

    HL7_ASSERT(parser != NULL && parser->user_data != NULL);

    state = (Decode_State *) parser->user_data;

    state->log = fopen("ehl7_decode.log", "a");
    if (state->log != NULL)
    {
        DEBUG("Started parsing HL7 message (callback)\n");
    }
    else
    {
        fprintf(stderr, "Could not open log file 'ehl7_decode.log'\n");
        rc = -1;
    }
#endif

    return rc;
}

/* ------------------------------------------------------------------------ */
static int end_document(UNUSED HL7_Parser *parser)
{
    int rc = 0;

#ifndef NDEBUG
    Decode_State *state;

    HL7_ASSERT(parser != NULL && parser->user_data != NULL);

    state = (Decode_State *) parser->user_data;

    HL7_ASSERT(state->log != NULL);

    DEBUG("Finished parsing HL7 message (callback)\n");

    fclose(state->log);
    state->log = NULL;
#endif

    return rc;
}

/* ------------------------------------------------------------------------ */
static int start_element(HL7_Parser *parser, HL7_Element_Type element_type)
{
    int rc = 0;
    Decode_State *state;

    HL7_ASSERT(parser != NULL && parser->user_data != NULL);
    HL7_ASSERT(element_type < HL7_ELEMENT_TYPE_COUNT);

    state = (Decode_State *) parser->user_data;

    DEBUG1("<%s>\n", element_type_name(element_type));

    /* Clean up the data that was added to the parent element in the previous iteration */
    memset(&state->term[element_type][0], 0, state->term_count[element_type] * sizeof (ERL_NIF_TERM));
    state->term_count[element_type] = 0;

    return rc;
}

/* ------------------------------------------------------------------------ */
static int end_element(HL7_Parser *parser, HL7_Element_Type element_type)
{
    int rc = 0;
    Decode_State *state;
    HL7_Element_Type parent_type;
    ERL_NIF_TERM term;

    HL7_ASSERT(parser != NULL && parser->user_data != NULL);
    HL7_ASSERT(element_type < HL7_ELEMENT_TYPE_COUNT);

    state = (Decode_State *) parser->user_data;
    parent_type = parent(element_type);

    if (state->term_count[element_type] > 1)
    {
        DEBUG1("adding list of %d elements to parent\n", state->term_count[element_type]);

        /* Create a tuple based on the array of terms that were accumulated and
           add it to the parent element */
        term = enif_make_tuple_from_array(state->env, &state->term[element_type][0],
                                          state->term_count[element_type]);
    }
    else if (state->term_count[element_type] == 1)
    {
        DEBUG("adding 1 element to parent as binary\n");

        /* If the element is a tuple we have to enclose it in another tuple to
           accurately represent the hierarchy of repetitions, components and
           subcomponents. */
        if (enif_is_tuple(state->env, state->term[element_type][0]))
        {
            term = enif_make_tuple1(state->env, state->term[element_type][0]);
        }
        else
        {
            term = state->term[element_type][0];
        }
    }
    else /* if (state->term_count[element_type] == 0) */
    {
        /* ErlNifBinary empty_binary; */

        DEBUG("adding 'undefined' atom to parent\n");

        /* There were no elements; add an empty binary to the parent element */
        /*
        enif_alloc_binary(0, &empty_binary);
        term = enif_make_binary(state->env, &empty_binary);
        */
        term = state->undefined_atom;
    }
    state->term[parent_type][state->term_count[parent_type]] = term;
    ++state->term_count[parent_type];

    DEBUG1("</%s>\n", element_type_name(element_type));

    return rc;
}


/* ------------------------------------------------------------------------ */
static int characters(HL7_Parser *parser, HL7_Element_Type element_type, HL7_Element *element)
{
    int rc = 0;
    Decode_State *state;
    ErlNifBinary binary;
    ERL_NIF_TERM term;

    HL7_ASSERT(parser != NULL && parser->user_data != NULL);
    HL7_ASSERT(element_type < HL7_ELEMENT_TYPE_COUNT);
    HL7_ASSERT(element != NULL);

    state = (Decode_State *) parser->user_data;

    DEBUG3("  %.*s (%u bytes): ", (unsigned) element->length, (element->value != NULL ? element->value : "<NULL>"),
           (int) element->length);

    if (!hl7_element_is_empty(element))
    {
        enif_alloc_binary(element->length, &binary);
        memcpy(binary.data, element->value, element->length);
        term = enif_make_binary(state->env, &binary);
    }
    else
    {
        term = state->undefined_atom;
    }
    state->term[element_type][state->term_count[element_type]] = term;
    ++state->term_count[element_type];

    return rc;
}


/* ------------------------------------------------------------------------ */
static int encode_segment(Encode_State *state, HL7_Parser *parser, HL7_Allocator *allocator,
                          HL7_Buffer *buffer, ERL_NIF_TERM current)
{
    int                 rc = -1;
    int                 tuple_size;
    const ERL_NIF_TERM  *tuple;
    ErlNifBinary        segment_id;
    HL7_Segment         segment;
    HL7_Element_Type    element_type = HL7_ELEMENT_FIELD;
    size_t              index[HL7_ELEMENT_TYPE_COUNT];

    if (enif_get_tuple(state->env, current, &tuple_size, &tuple))
    {
        if (enif_inspect_binary(state->env, tuple[0], &segment_id))
        {
            char *segment_id_str = (char *) alloca(segment_id.size + 1);

            memcpy(segment_id_str, segment_id.data, segment_id.size);
            segment_id_str[segment_id.size] = '\0';

            memset(index, 0, sizeof (index));

            DEBUG1("Encoding segment %s\n", segment_id_str);
            if (hl7_segment_create(&segment, segment_id_str, allocator) == 0)
            {
                /* Skip over the segment ID when encoding the tuple representing the segment */
                if (encode_tuple(state, &segment, element_type, index, tuple_size - 1, tuple + 1) == 0)
                {
                    rc = hl7_parser_write_segment(parser, buffer, &segment);
                }
            }
        }
    }
    return rc;
}


/* ------------------------------------------------------------------------ */
static int encode_tuple(Encode_State *state, HL7_Segment *segment, HL7_Element_Type element_type, size_t *index,
                        int tuple_size, const ERL_NIF_TERM *tuple)
{
    int rc = 0;

    if (element_type >= 0 && element_type < HL7_ELEMENT_TYPE_COUNT)
    {
        ErlNifBinary bin;
        HL7_Element element;
        int child_size;
        const ERL_NIF_TERM *child;
        int i;

        for (i = 0; i < tuple_size; ++i)
        {
            /* Set the index of the current element */
            index[element_type] = i;

            if (enif_inspect_binary(state->env, tuple[i], &bin))
            {
                hl7_element_set_ptr(&element, (char *) bin.data, bin.size, false);
                DEBUG4("  %s %d: %s (binary, %u bytes)\n", element_type_name(element_type), i,
                       (element.value != NULL ? element.value : "<NULL>"), (unsigned) element.length);
                set_element(segment, element_type, index, &element);
            }
            else if (enif_is_tuple(state->env, tuple[i]))
            {
                if (enif_get_tuple(state->env, tuple[i], &child_size, &child))
                {
                    DEBUG3("  %s %d: %u elements (tuple)\n", element_type_name(element_type), i, (unsigned) child_size);
                    encode_tuple(state, segment, hl7_child_type(element_type), index, child_size, child);
                }
                else
                {
                    rc = -1;
                    break;
                }
            }
            else /* if (enif_is_atom(state->env, tuple[i])) */
            {
                DEBUG2("  %s %d: skipping; atom or invalid type\n", element_type_name(element_type), i);
                /* Atoms are copied as empty elements, as the only atom we allow is 'undefined' */
                hl7_element_set_ptr(&element, NULL, 0, false);
                if (set_element(segment, element_type, index, &element) != 0)
                {
                    rc = -1;
                    break;
                }
            }
        }
    }
    else
    {
        rc = -1;
    }
    return rc;
}


/* ------------------------------------------------------------------------ */
static int set_element(HL7_Segment *segment, HL7_Element_Type element_type, size_t *index, HL7_Element *element)
{
    int rc = 0;

    switch (element_type)
    {
    case HL7_ELEMENT_FIELD:
        rc = hl7_segment_set_field(segment, index[HL7_ELEMENT_FIELD], element);
        break;
    case HL7_ELEMENT_REPETITION:
        rc = hl7_segment_set_repetition(segment, index[HL7_ELEMENT_FIELD], index[HL7_ELEMENT_REPETITION], element);
        break;
    case HL7_ELEMENT_COMPONENT:
        rc = hl7_segment_set_component_rep(segment, index[HL7_ELEMENT_FIELD], index[HL7_ELEMENT_REPETITION],
                                           index[HL7_ELEMENT_COMPONENT], element);
        break;
    case HL7_ELEMENT_SUBCOMPONENT:
        rc = hl7_segment_set_subcomponent_rep(segment, index[HL7_ELEMENT_FIELD], index[HL7_ELEMENT_REPETITION],
                                              index[HL7_ELEMENT_COMPONENT], index[HL7_ELEMENT_SUBCOMPONENT], element);
        break;
    default:
        rc = -1;
    }
    return rc;
}


/* ------------------------------------------------------------------------ */
static ERL_NIF_TERM nif_error(ErlNifEnv *env, const char *reason)
{
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, reason));
}


/* ------------------------------------------------------------------------ */
static ERL_NIF_TERM nif_error_data(ErlNifEnv *env, const char *reason, ERL_NIF_TERM data)
{
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
                            enif_make_tuple2(env, enif_make_atom(env, reason), data));
}


/* ------------------------------------------------------------------------ */
static int parent(HL7_Element_Type element_type)
{
    HL7_ASSERT(element_type <= HL7_ELEMENT_SEGMENT);

    return element_type + 1;
}


#ifndef NDEBUG

/* ------------------------------------------------------------------------ */
static char *element_type_name(const HL7_Element_Type element_type)
{
    static char *ELEMENT_TYPE_NAME[HL7_ELEMENT_TYPE_COUNT] = {
        "subcomponent",
        "component",
        "repetition",
        "field",
        "segment"
    };

    return (element_type < HL7_ELEMENT_TYPE_COUNT ? ELEMENT_TYPE_NAME[(int) element_type] : "Unknown");
}

#endif
