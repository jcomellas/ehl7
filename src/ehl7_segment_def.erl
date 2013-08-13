%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc Module that generates code to decode/encode HL7 segments.
%%% @end
%%%-------------------------------------------------------------------
-module(ehl7_segment_def).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([codegen/0, codegen/1, get_segment_defs/1]).

-type element_name() :: atom().
-type element_type() :: 'field' | 'repetition' | 'component' | 'subcomponent'.
-type element_index() :: non_neg_integer() | [non_neg_integer()].
-type element_data_type() :: 'integer' | 'float' | 'date' | 'string'.

-type element_def() :: {element_name(), element_type(), element_index(), element_data_type(), non_neg_integer()}.
-type segment_def() :: {ehl7:raw_segment_id(), Comment :: binary(), [element_def()]}.

-type codegen_option() :: [{segment_def, SegmentDefFile :: string()} |
                           {header, HeaderFile :: string()} |
                           {source, SourceFile :: string()} |
                           verbose].

-define(DEFAULT_SEGMENT_DEF_FILE, "src/ehl7_segment_def.eon").
-define(DEFAULT_HEADER_FILE, "include/ehl7_segment.hrl").
-define(DEFAULT_SOURCE_FILE, "src/ehl7_segment.erl").

-define(START_BRACE, "[").
-define(END_BRACE, "]").


-spec codegen() ->ok.
codegen() ->
    codegen([]).


-spec codegen(codegen_option()) -> ok.
codegen(Options) ->
    SegmentDefFile = proplists:get_value(segment_def, Options, ?DEFAULT_SEGMENT_DEF_FILE),
    HeaderFile = proplists:get_value(header, Options, ?DEFAULT_HEADER_FILE),
    SourceFile = proplists:get_value(source, Options, ?DEFAULT_SOURCE_FILE),
    Verbose = lists:member(verbose, Options),
    case get_segment_defs(SegmentDefFile) of
        {ok, SegmentDefs} ->
            case Verbose of
                true ->
                    io:format("Generating header in file ~s~n", [HeaderFile]);
                false ->
                    void
            end,
            file:write_file(HeaderFile, codegen_segment_header(HeaderFile, SourceFile, SegmentDefs)),
            case Verbose of
                true ->
                    io:format("Generating source in file ~s~n", [SourceFile]);
                false ->
                    void
            end,
            file:write_file(SourceFile, codegen_segment_source(HeaderFile, SourceFile, SegmentDefs));
        Error ->
            Error
    end.


-spec codegen_segment_header(HeaderFile :: string(), SourceFile :: string(), [segment_def()]) -> iolist().
codegen_segment_header(HeaderFile, SourceFile, SegmentDefs) ->
    [
     file_header(), $\n,
     codegen_segment_header(HeaderFile, SourceFile, SegmentDefs, [])
    ].

codegen_segment_header(HeaderFile, SourceFile, [SegmentDef | Tail], Acc) ->
    codegen_segment_header(HeaderFile, SourceFile, Tail, [codegen_segment_record(SegmentDef) | Acc]);
codegen_segment_header(_HeaderFile, _SourceFile, [], Acc) ->
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


codegen_segment_source(HeaderFile, SourceFile, SegmentDefs) ->
    Prefix = [
              file_header(),
              <<"-module(">>, filename:basename(SourceFile, ".erl"), <<").\n">>,
              <<"-author('">>, file_author(), <<"').\n\n">>,
              <<"-include(\"">>, HeaderFile, <<"\").\n\n">>,
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
    <<"Juan Jose Comellas <juanjo@comellas.org>">>.


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
       "%%%-------------------------------------------------------------------\n"
     >>
    ].


-spec get_segment_defs(SegmentDefFile :: string()) -> [segment_def()].
get_segment_defs(SegmentDefFile) ->
    case file:consult(SegmentDefFile) of
        {ok, _SegmentDefs} = Result ->
            Result;
        {error, Reason} ->
            {error, {Reason, SegmentDefFile}}
    end.
