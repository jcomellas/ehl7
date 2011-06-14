%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <jcomellas@erlar.com>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc Module that generates code to decode/encode HL7 segments.
%%% @end
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(ehl7_segment_def).
-author('Juan Jose Comellas <jcomellas@erlar.com>').

-export([main/1]).
-export([codegen/1, get_segment_defs/0]).

-type segment_id() :: binary().
-type element_name() :: atom().
-type element_type() :: 'field' | 'repetition' | 'component' | 'subcomponent'.
-type element_index() :: non_neg_integer() | [non_neg_integer()].
-type element_data_type() :: 'integer' | 'float' | 'date' | 'string'.

-type element_def() :: {element_name(), element_type(), element_index(), element_data_type(), non_neg_integer()}.
-type segment_def() :: {segment_id(), Comment :: binary(), [element_def()]}.

-define(START_BRACE, "[").
-define(END_BRACE, "]").


main(Args) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, _NonOptArgs}} ->
            case lists:member(help, Options) of
                true ->
                    getopt:usage(OptSpecList, escript:script_name());
                false ->
                    codegen(Options)
            end;
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            getopt:usage(OptSpecList, escript:script_name())
    end.


option_spec_list() ->
    [
     %% {Name,     ShortOpt,  LongOpt,       ArgSpec,                      HelpMsg}
     {help,        $?,        "help",        undefined,
      "Show the program options"},
     {verbose,     $v,        "verbose",     {boolean, false},
      "Verbose output"},
     {header,      $h,        "hrl",         {string, "include/ehl7_segment.hrl"},
      "File where the generated header will be saved to"},
     {source,      $e,        "erl",         {string, "src/ehl7_segment.erl"},
      "File where the generated code will be saved to"}
    ].


codegen(Options) ->
    Header = proplists:get_value(header, Options),
    Source = proplists:get_value(source, Options),
    io:format("Generating header in file ~s~n", [Header]),
    file:write_file(Header, codegen_segment_header(Header, Source)),
    io:format("Generating source in file ~s~n", [Source]),
    file:write_file(Source, codegen_segment_source(Header, Source)).


codegen_segment_header(Header, Source) ->
    codegen_segment_header(Header, Source, get_segment_defs()).


-spec codegen_segment_header(Header :: string(), Source :: string(), [segment_def()]) -> iolist().
codegen_segment_header(Header, Source, SegmentDefs) ->
    [
     file_header(), $\n,
     codegen_segment_header(Header, Source, SegmentDefs, [])
    ].

codegen_segment_header(Header, Source, [SegmentDef | Tail], Acc) ->
    codegen_segment_header(Header, Source, Tail, [codegen_segment_record(SegmentDef) | Acc]);
codegen_segment_header(_Header, _Source, [], Acc) ->
    lists:reverse(Acc).


-spec codegen_segment_record(segment_def()) -> iolist().
codegen_segment_record({SegmentId, Description, ElementDefs}) ->
    RecordName = bstr:lower(SegmentId),
    [<<"%% ", SegmentId/binary, ": ", Description/binary, $\n>>,
     <<"-record(", RecordName/binary, ", {\n">> | codegen_segment_record_1(ElementDefs, [])].

-spec codegen_segment_record_1([element_def()], list()) -> iolist().
codegen_segment_record_1([{Name, _Type, _Index, DataType, Length} | Tail], Acc) ->
    Indentation = <<"          ">>,
    LineEnd = case Tail of
                  [] ->
                      <<"\n         }).\n\n">>;
                  _ ->
                      <<",\n">>
              end,
    codegen_segment_record_1(Tail, [[Indentation, atom_to_binary(Name, latin1), type_spec(DataType, Length), LineEnd] | Acc]);
codegen_segment_record_1([], Acc) ->
    lists:reverse(Acc).


codegen_segment_source(Header, Source) ->
    codegen_segment_source(Header, Source, get_segment_defs()).

codegen_segment_source(Header, Source, SegmentDefs) ->
    Prefix = [
              file_header(),
              <<"-module(">>, filename:basename(Source, ".erl"), <<").\n">>,
              <<"-author('">>, file_author(), <<"').\n\n">>,
              <<"-include(\"">>, Header, <<"\").\n\n">>,
              <<
                "-import(ehl7_field, [field/4, component/4, subcomponent/4, to_raw_value/3]).\n"
                "-export([decode/1, encode/1, encode/2]).\n\n"
                "-type encode_options() :: 'tuple' | 'list'.\n\n\n"
                "%% @doc Decode a segment encoded as a tuple and convert it to a record\n"
                "-spec decode(ehl7:raw_segment()) -> ehl7:segment().\n"
                "decode(RawSegment) ->\n"
                "    SegmentId = binary_to_atom(bstr:lower(element(1, RawSegment)), latin1),\n"
                "    decode(SegmentId, RawSegment).\n\n\n"
                "%% @doc Encode a segment record and convert it to a tuple\n"
                "-spec encode(ehl7:segment()) -> ehl7:raw_segment().\n"
                "encode(Segment) ->\n"
                "    SegmentId = element(1, Segment),\n"
                "    encode_as_tuple(SegmentId, Segment).\n\n\n"
                "%% @doc Encode a segment record and convert it to a list or tuple depending on the Options\n"
                "-spec encode(ehl7:segment(), encode_options()) -> ehl7:raw_segment().\n"
                "encode(Segment, Options) ->\n"
                "    SegmentId = element(1, Segment),\n"
                "    case lists:member(list, Options) of\n"
                "        true ->\n"
                "            encode_as_list(SegmentId, Segment);\n"
                "        false ->\n"
                "            encode_as_tuple(SegmentId, Segment)\n"
                "    end.\n\n\n"
              >>
             ],
    [
     Prefix,
     codegen_segment_decoder(SegmentDefs, []),
     codegen_segment_encoder(<<"encode_as_tuple">>, tuple, SegmentDefs, []),
     codegen_segment_encoder(<<"encode_as_list">>, list, SegmentDefs, [])
    ].


codegen_segment_decoder([{SegmentId, Description, ElementDefs} | Tail], Acc) ->
    RecordName = bstr:lower(SegmentId),
    Decoder = [
               [<<"%% @doc Decode the ">>, SegmentId, <<" (">>, Description, <<") segment\n">>],
               [<<"decode(">>, RecordName, <<", Segment) ->\n">>],
               [<<"    #">>, RecordName, <<"{\n">>] |
               codegen_element_decoder(ElementDefs, [])
              ],
    codegen_segment_decoder(Tail, [Decoder | Acc]);
codegen_segment_decoder([], Acc) ->
    Decoder  = [
                <<"decode(_SegmentId, Segment) ->\n">>,
                <<"    {error, {unknown_segment_id, element(1, Segment)}}.\n\n\n">>
               ],
    lists:reverse([Decoder | Acc]).


codegen_element_decoder([{Name, Type, Index, DataType, Length} | Tail], Acc) ->
    Indentation = <<"        ">>,
    LineEnd = case Tail of
                  [] ->
                      <<"\n       };\n">>;
                  _ ->
                      <<",\n">>
              end,
    Line = io_lib:format("~s = ~s(~w, ~s, ~w, Segment)", [Name, Type, Index, DataType, Length]),
    codegen_element_decoder(Tail, [[Indentation, Line, LineEnd] | Acc]);
codegen_element_decoder([], Acc) ->
    lists:reverse(Acc).



codegen_segment_encoder(FunctionName, Format, [{SegmentId, Description, ElementDefs} | Tail], Acc) ->
    %% io:format("Generating encoder for segment ~s~n", [SegmentId]),
    LowerSegmentId = bstr:lower(SegmentId),
    LineEnd = case Tail of
                  [] ->
                      <<".\n\n\n">>;
                  _ ->
                      <<";\n">>
              end,
    {StartBrace, EndBrace, FormatStr} = case Format of
                                            tuple ->
                                                {${, $}, atom_to_binary(Format, latin1)};
                                            list ->
                                                {$[, $], atom_to_binary(Format, latin1)}
                                        end,
    Encoder = [
               [
                <<"%% @doc Encode the ">>, SegmentId, <<" (">>, Description, <<") segment as a ", FormatStr/binary, "\n">>,
                <<FunctionName/binary, "(">>, LowerSegmentId,
                <<", Segment) ->\n"
                  "    ", StartBrace, "\n"
                  "      <<\"">>, SegmentId, <<"\">>,\n">>
               ],
               codegen_element_encoder(LowerSegmentId, StartBrace, EndBrace, [0], ElementDefs, []),
               <<"    ", EndBrace>>, LineEnd
              ],
    codegen_segment_encoder(FunctionName, Format, Tail, [Encoder | Acc]);
codegen_segment_encoder(_FunctionName, _Format, [], Acc) ->
    lists:reverse(Acc).


codegen_element_encoder(SegmentId, StartBrace, EndBrace, PrevIndex, [{Name, _Type, Index, DataType, Length} | Tail], Acc) ->
    {EndBraceCount, StartBraceCount, Depth, MissingCount} = get_brace_count(PrevIndex, Index),
    %% io:format("Generating encoder for element ~s.~s (EndBraceCount=~w, StartBraceCount=~w, Depth=~w, Missing=~w)~n",
    %%           [SegmentId, Name, EndBraceCount, StartBraceCount, Depth, MissingCount]),
    Indentation = indentation(Depth),
    {PrevDepth, Separator} = case PrevIndex of
                                 [0] ->
                                     {1, <<>>};
                                 _ ->
                                     {length(PrevIndex), add_ending_separator(EndBraceCount, [])}
                             end,
    MissingElements = case MissingCount of
                          0 ->
                              [];
                          _ ->
                              [indentation(PrevDepth - EndBraceCount), <<"undefined">>,
                               lists:duplicate(MissingCount - 1, <<", undefined">>), <<",\n">>]
                      end,
    Encoder = [
               Separator,
               add_ending_braces(EndBrace, EndBraceCount, length(PrevIndex), first, []),
               MissingElements,
               add_starting_braces(StartBrace, StartBraceCount, length(Index), []),
               [Indentation, io_lib:format("%% ~w\n", [Index])],
               [Indentation, io_lib:format("to_raw_value(Segment#~s.~s, ~s, ~w)",
                                           [SegmentId, Name, DataType, Length])]
              ],
    codegen_element_encoder(SegmentId, StartBrace, EndBrace, Index, Tail, [Encoder | Acc]);
codegen_element_encoder(_SegmentId, _StartBrace, EndBrace, PrevIndex, [], Acc) ->
    {EndBraceCount, _StartBraceCount, _Depth, _Missing} = get_brace_count(PrevIndex, [0]),
    [lists:reverse(Acc), <<"\n">>, add_ending_braces(EndBrace, EndBraceCount, length(PrevIndex), last, [])].


add_ending_separator(EndBraceCount, Acc) ->
    Separator = case EndBraceCount > 0 of
                    true ->
                        <<"\n">>;
                    false ->
                        <<",\n">>
                end,
    [Separator | Acc].


add_starting_braces(StartBrace, Count, Depth, Acc) when Count > 0 ->
    Line = [indentation(Depth - Count), <<StartBrace, $\n>>],
    add_starting_braces(StartBrace, Count - 1, Depth, [Line | Acc]);
add_starting_braces(_StartBrace, _Count, _Depth, Acc) ->
    lists:reverse(Acc).

add_ending_braces(EndBrace, Count, Depth, Section, Acc) when Count > 0 ->
    LineEnd = if
                  Count > 1 orelse Section =:= last ->
                      <<EndBrace, "\n">>;
                  true ->
                      <<EndBrace, ",\n">>
              end,
    Line = [indentation(Depth - 1), LineEnd],
    add_ending_braces(EndBrace, Count - 1, Depth - 1, Section, [Line | Acc]);
add_ending_braces(_EndBrace, _Count, _Depth, _Section, Acc) ->
    lists:reverse(Acc).


get_brace_count(List1, List2) ->
    get_brace_count(length(List1), length(List2), List1, List2, 0).

get_brace_count(Length1, Length2, [Head1 | Tail1], [Head1 | Tail2], MatchCount) ->
    get_brace_count(Length1, Length2, Tail1, Tail2, MatchCount + 1);
get_brace_count(Length1, Length2, Index1, Index2, MatchCount) ->
    Missing = case Index2 of
                  0 ->
                      0;
                  _ ->
                      hd(Index2) - hd(Index1) - 1
              end,
    case MatchCount > 0 of
        true ->
            if
                Length1 < Length2 ->
                    {0, Length2 - Length1, Length2, Missing};
                true ->
                    {Length1 - Length2, 0, Length2, Missing}
            end;
        false ->
            if
                Length1 < Length2 ->
                    {0, Length2 - Length1, Length2, Missing};
                Length1 =:= Length2 ->
                    {Length1 - 1, Length2 - 1, Length2, Missing};
                true ->
                    {Length1 - 1, Length2 - 1, Length2, Missing}
            end
    end.


-spec indentation(Depth :: non_neg_integer()) -> binary().
indentation(Depth) ->
    bstr:duplicate($\s, 4 + Depth * 2).


-spec type_spec(element_data_type(), non_neg_integer()) -> binary().
type_spec(integer, _Length) ->
    <<" :: integer() | undefined">>;
type_spec(float, _Length) ->
    <<" :: float() | undefined">>;
type_spec(date, 8) ->
    <<" :: calendar:date() | undefined">>;
type_spec(date, Length) when Length > 8 ->
    <<" :: calendar:datetime() | undefined">>;
type_spec(string, _Length) ->
    <<" :: binary() | undefined">>.


-spec file_author() -> binary().
file_author() ->
    <<"Juan Jose Comellas <jcomellas@erlar.com>">>.


-spec file_copyright() -> binary().
file_copyright() ->
    <<"(C) 2011 Juan Jose Comellas">>.


-spec file_header() -> binary().
file_header() ->
    [
     <<
       "%%%-------------------------------------------------------------------\n"
       "%%% @author "
     >>, file_author(), $\n,
     <<
       "%%% @copyright "
     >>, file_copyright(), $\n,
     <<
       "%%% @doc Module that parses and generates segments for HL7 messages.\n"
       "%%% @end\n"
       "%%%\n"
       "%%% This file was autogenerated. DO NOT MODIFY.\n"
       "%%%\n"
       "%%% This source file is subject to the New BSD License. You should have received\n"
       "%%% a copy of the New BSD license with this software. If not, it can be\n"
       "%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php\n"
       "%%%-------------------------------------------------------------------\n"
     >>
    ].


-spec get_segment_defs() -> [segment_def()].
get_segment_defs() ->
    [
     %% AUT: Authorization information
     {<<"AUT">>, <<"Authorization information">>,
      [
       {plan_id,                               component,    [1, 1, 1],    string,   10},
       {plan_name,                             component,    [1, 1, 2],    string,   20},
       {company_id,                            component,    [2, 1, 1],    string,    6},
       {company_name,                          component,    [2, 1, 2],    string,   30},
       {company_id_coding_system,              component,    [2, 1, 3],    string,   20},
       {start_date,                            field,        [4],          date,      8},
       {end_date,                              field,        [5],          date,      8},
       {authorization_id,                      component,    [6, 1, 1],    string,   20},
       {requested_treatments,                  field,        [8],          integer,   2},
       {authorized_treatments,                 field,        [9],          integer,   2}
      ]},
     %% DG1: Diagnosis information
     {<<"DG1">>, <<"Diagnosis information">>,
      [
       {set_id,                                field,       [1],           integer,   4},
       {diagnosis_id,                          component,   [3, 1, 1],     string,   20},
       {name,                                  component,   [3, 1, 2],     string,   32},
       {coding_system,                         component,   [3, 1, 3],     string,   10},
       {diagnosis_type,                        field,       [6],           string,    2}
      ]},
     %% DSC: Continuation pointer
     {<<"DSC">>, <<"Continuation pointer">>,
      [
       {continuation_pointer,                  field,        [1],          string,   15}
      ]},
     %% DSP: Display data
     {<<"DSP">>, <<"Display data">>,
      [
       {set_id,                                field,        [1],          integer,   4},
       {display_level,                         field,        [2],          string,    4},
       {data_line,                             field,        [3],          string,   40},
       {break_point,                           field,        [4],          string,    2},
       {result_id,                             field,        [5],          string,   20}
      ]},
     %% ERR: Error information
     {<<"ERR">>, <<"Error information">>,
      [
       {segment_id,                            component,    [1, 1, 1],    string,    3},
       {sequence,                              component,    [1, 1, 2],    integer,   3},
       {field_pos,                             component,    [1, 1, 3],    integer,   3},
       {error_code,                            subcomponent, [1, 1, 4, 1], string,    9},
       {error_text,                            subcomponent, [1, 1, 4, 2], string,   61}
      ]},
     %% EVN: Event type
     {<<"EVN">>, <<"Event type">>,
      [
       {recorded_date,                         field,        [2],          date,     14},
       {planned_event_date,                    field,        [3],          date,     14},
       {event_reason_code,                     field,        [4],          string,    3}
      ]},
     %% IN1: Insurance
     {<<"IN1">>, <<"Insurance">>,
      [
       {set_id,                                field,        [1],          integer,   4},
       {plan_id,                               component,    [2, 1, 1],    string,   20},
       {plan_name,                             component,    [2, 1, 2],    string,   30},
       {company_id,                            component,    [3, 1, 1],    string,   6},
       {company_assigning_authority_id,        subcomponent, [3, 1, 4, 1], string,   10},
       {company_id_type,                       subcomponent, [3, 1, 4, 5], string,   10},
       {authorization_number,                  component,    [14, 1, 1],   string,   20},
       {auhtorization_date,                    component,    [14, 1, 2],   date,      8}
      ]},
     %% MSA: Message acknowledgment
     {<<"MSA">>, <<"Message acknowledgment">>,
      [
       {ack_code,                              field,        [1],          string,    2},
       {message_control_id,                    field,        [2],          string,   20},
       {error_code,                            component,    [6, 1, 1],    string,   10},
       {error_text,                            component,    [6, 1, 2],    string,   40}
      ]},
     %% MSH: Message header
     {<<"MSH">>, <<"Message header">>,
      [
       {field_separator,                       field,        [1],          string,    1},
       {encoding_characters,                   field,        [2],          string,    4},
       {sending_application_id,                component,    [3, 1, 1],    string,   12},
       {sending_facility_id,                   component,    [4, 1, 1],    string,   12},
       {sending_facility_universal_id,         component,    [4, 1, 2],    string,   20},
       {sending_facility_universal_id_type,    component,    [4, 1, 3],    string,   20},
       {receiving_application_id,              component,    [5, 1, 1],    string,   12},
       {receiving_facility_id,                 component,    [6, 1, 1],    string,   12},
       {receiving_facility_universal_id,       component,    [6, 1, 2],    string,   20},
       {receiving_facility_universal_id_type,  component,    [6, 1, 3],    string,   20},
       {message_date,                          field,        [7],          date,     14},
       {message_type,                          component,    [9, 1, 1],    string,    3},
       {trigger_event,                         component,    [9, 1, 2],    string,    3},
       {message_structure,                     component,    [9, 1, 3],    string,    7},
       {message_control_id,                    field,        [10],         string,   20},
       {processing_id,                         field,        [11],         string,    3},
       {version,                               field,        [12],         string,    8},
       {accept_ack_type,                       field,        [15],         string,    2},
       {application_ack_type,                  field,        [16],         string,    2},
       {country_code,                          field,        [17],         string,    3}
      ]},
     %% NTE: Notes and comments
     {<<"NTE">>, <<"Notes and comments">>,
      [
       {set_id,                                field,        [1],          integer,   4},
       {comment,                               field,        [3],          string,  512}
      ]},
     %% PID: Patient information
     {<<"PID">>, <<"Patient information">>,
      [
       {set_id,                                field,        [1],          integer,   4},
       %% Repetition 1
       {patient_id,                            component,    [3, 1, 1],    string,   20},
       {assigning_authority_id,                subcomponent, [3, 1, 4, 1], string,    6},
       {assigning_authority_universal_id,      subcomponent, [3, 1, 4, 2], string,    6},
       {assigning_authority_universal_id_type, subcomponent, [3, 1, 4, 3], string,   10},
       {id_type,                               component,    [3, 1, 5],    string,    2},
       %% Repetition 2
       {patient_document_id,                   component,    [3, 2, 1],    string,   20},
       {patient_document_id_type,              component,    [3, 2, 5],    string,    2},
       {last_name,                             component,    [5, 1, 1],    string,   25},
       {first_name,                            component,    [5, 1, 2],    string,   25}
      ]},
     %% PR1: Procedure information
     {<<"PR1">>, <<"Procedure information">>,
      [
       {set_id,                                field,        [1],          integer,   4},
       {procedure_id,                          component,    [3, 1, 1],    string,   20},
       {procedure_name,                        component,    [3, 1, 2],    string,   30},
       {coding_system,                         component,    [3, 1, 3],    string,    4},
       {date,                                  field,        [5],          date,     14}
      ]},
     %% PRD: Provider data
     {<<"PRD">>, <<"Provider data">>,
      [
       %% Repetition 1
       {role_id,                               component,    [1, 1, 1],    string,    5},
       {role_name,                             component,    [1, 1, 2],    string,   30},
       {role_coding_system,                    component,    [1, 1, 3],    string,    7},
       %% Repetition 2
       {specialty_id,                          component,    [1, 2, 1],    string,    5},
       {specialty_name,                        component,    [1, 2, 2],    string,   30},
       {specialty_coding_system,               component,    [1, 2, 3],    string,    7},
       {last_name,                             component,    [2, 1, 1],    string,   40},
       {first_name,                            component,    [2, 1, 2],    string,   30},
       {street,                                component,    [3, 1, 1],    string,   20},
       {other_designation,                     component,    [3, 1, 2],    string,   20},
       {city,                                  component,    [3, 1, 3],    string,   30},
       {state,                                 component,    [3, 1, 4],    string,    1},
       {postal_code,                           component,    [3, 1, 5],    string,   10},
       {country_code,                          component,    [3, 1, 6],    string,    3},
       {address_type,                          component,    [3, 1, 7],    string,    1},
       {provider_id,                           component,    [7, 1, 1],    string,   15},
       %% {provider_id_type,                      component,    [7, 1, 2],    string,    4},
       {provider_id_type,                      subcomponent, [7, 1, 2, 1], string,    2},
       {provider_id_type_medical,              subcomponent, [7, 1, 2, 2], string,    1},
       {provider_id_type_province,             subcomponent, [7, 1, 2, 3], string,    1},
       {provider_id_alternate_qualifier,       component,    [7, 1, 3],    string,    8}
      ]},
     %% PV1: Patient visit
     {<<"PV1">>, <<"Patient visit">>,
      [
       {set_id,                                field,        [1],          string,    4},
       {patient_class,                         field,        [2],          string,    1},
       %% {assigned_patient_location,             field,        [3],          string,   34},
       {patient_point_of_care,                 component,    [3, 1, 1],    string,   10},
       {patient_location_facility,             component,    [3, 1, 4],    string,   21},
       {admission_type,                        field,        [4],          string,   34},
       %% {attending_doctor,                      field,        [7],          string,   99},
       {attending_doctor_id,                   component,    [7, 1, 1],    string,   20},
       {attending_doctor_last_name,            component,    [7, 1, 2],    string,   25},
       {attending_doctor_first_name,           component,    [7, 1, 3],    string,   25},
       {attending_doctor_assigning_authority,  component,    [7, 1, 9],    string,   21},
       %% {referring_doctor,                      field,        [8],          string,   99},
       {referring_doctor_id,                   component,    [8, 1, 1],    string,   20},
       {referring_doctor_last_name,            component,    [8, 1, 2],    string,   25},
       {referring_doctor_first_name,           component,    [8, 1, 3],    string,   25},
       {referring_doctor_assigning_authority,  component,    [8, 1, 9],    string,   21},
       {hospital_service,                      field,        [10],         string,   99},
       {readmission_indicator,                 field,        [13],         string,    2},
       {discharge_diposition,                  field,        [36],         string,    3},
       {admit_date,                            field,        [44],         date,     12},
       {discharge_date,                        field,        [45],         date,     12},
       {visit_indicator,                       field,        [51],         string,    1}
      ]},
     %% PV2: Patient visit - additional information
     {<<"PV2">>, <<"Patient visit - additional information">>,
      [
       %% {transfer_reason,                       field,        [4],          string,   20},
       {transfer_reason_id,                    component,    [4, 1, 1],    string,   20}
      ]},
     %% QAK: Query acknowledgment
     {<<"QAK">>, <<"Query acknowledgment">>,
      [
       {query_tag,                             field,        [1],          string,   32},
       {query_response_status,                 field,        [2],          string,    4},
       {query_id,                              component,    [3, 1, 1],    string,   14},
       {query_name,                            component,    [3, 1, 2],    string,   30}
      ]},
     %% QPD: Query parameter definition
     {<<"QPD_Q15">>, <<"Query parameter definition -- procedure totals query">>,
      [
       %% {message_query_name,                    field,        [1],          string,   50},
       {query_id,                              component,    [1, 1, 1],    string,   20},
       {query_name,                            component,    [1, 1, 2],    string,   30},
       {query_tag,                             field,        [2],          string,   32},
       {provider_id,                           component,    [3, 1, 1],    string,   15},
       {provider_id_type,                      component,    [3, 1, 2],    string,    4},
       {start_date,                            field,        [4],          date,     12},
       {end_date,                              field,        [5],          date,     12},
       {procedure_id,                          component,    [6, 1, 1],    string,   30},
       {procedure_coding_system,               component,    [6, 1, 2],    string,    8},
       {authorizer_id,                         component,    [7, 1, 1],    string,    6}
      ]},
     %% RCP: Response control parameter
     {<<"RCP">>, <<"Response control parameter">>,
      [
       {query_priority,                        field,        [1],          string,    1},
       {response_limit,                        component,    [2, 1, 1],    integer,  10},
       {response_unit,                         subcomponent, [2, 1, 2, 1], string,    2},
       {response_modality_id,                  component,    [3, 1, 1],    string,   10},
       {execution_date,                        field,        [4],          date,     12},
       {sort_by,                               field,        [6],          string,   512}
      ]},
     %% RF1: Referral information
     {<<"RF1">>, <<"Referral information">>,
      [
       %% {referral_status,                       field,        [1],          string,   21},
       {referral_status_id,                    component,    [1, 1, 1],    string,    5},
       {referral_status_description,           component,    [1, 1, 2],    string,   15},
       %% {referral_type,                         field,        [3],          string,   21},
       {referral_type_id,                      component,    [3, 1, 1],    string,    5},
       {referral_type_description,             component,    [3, 1, 2],    string,   15},
       %% {originating_referral_id,               field,        [6, 1, 1],    string,   15},
       {originating_referral_id,               component,    [6, 1, 1],    string,   15},
       {effective_date,                        field,        [7],          date,     12},
       {expiration_date,                       field,        [8],          date,     12},
       {process_date,                          field,        [9],          date,     12},
       %% {referral_reason,                       field,        [10],         string,   21},
       {referral_reason_id,                    component,    [10, 1, 1],   string,   21}
      ]},
     %% ZAU: Procedure authorization information
     {<<"ZAU">>, <<"Procedure authorization information">>,
      [
       {prev_authorization_id,                 component,    [1, 1, 1],    string,   15},
       {payor_control_id,                      component,    [2, 1, 1],    string,   15},
       {authorization_status,                  component,    [3, 1, 1],    string,    4},
       {authorization_status_text,             component,    [3, 1, 2],    string,   15},
       {pre_authorization_id,                  component,    [4, 1, 1],    string,   15},
       {pre_authorization_date,                field,        [5],          string,    8},
       {copay,                                 subcomponent, [6, 1, 1, 1], float,    10},
       {copay_currency,                        subcomponent, [6, 1, 1, 2], string,   10}
      ]},
     %% ZIN: Additional insurance information
     {<<"ZIN">>, <<"Additional insurance information">>,
      [
       {eligibility_indicator,                 field,        [1],          string,    1},
       {patient_vat_status,                    component,    [2, 1, 1],    string,    4},
       {patient_vat_status_text,               component,    [2, 1, 2],    string,    7}
      ]}
    ].
