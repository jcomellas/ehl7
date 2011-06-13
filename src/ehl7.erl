%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <jcomellas@erlar.com>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc Module that parses and generates HL7 messages.
%%% @end
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(ehl7).
-author('Juan Jose Comellas <juanjo@comellas.org>').

%% API
-export([decode/2, encode/2, raw_encode/1]).
-export([segment/2, segment/3, segment_count/2]).
-export([field/2, field/4]).

-export_type([raw_msg/0, msg/0, raw_segment_id/0, segment_id/0, raw_segment/0, segment/0,
              raw_field/0, field/0, field_index/0, field_data_type/0, field_length/0]).

-on_load(init/0).

-type raw_msg() :: [tuple()].
-type msg() :: [tuple()].
-type raw_segment_id() :: binary().
-type segment_id() :: atom().
-type raw_segment() :: tuple().
-type segment() :: tuple().
-type raw_field() :: binary().
-type field() :: tuple() | binary() | integer() | calendar:date() | calendar:datetime() | float().
-type field_index() :: non_neg_integer() | [non_neg_integer()].
-type field_data_type() :: 'string' | 'integer' | 'date' | 'float'.
-type field_length() :: non_neg_integer().
-type decode_option() :: 'raw'.
-type encode_option() :: 'raw' | 'no_simplify'.


%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok | {error, Reason :: any()}.
init() ->
    erlang:load_nif("priv/ehl7", 0).


-spec decode(Buffer :: binary(), [decode_option()]) -> {ok, raw_msg()} | {error, Reason :: any()}.
decode(Buffer, Options) ->
    case raw_decode(Buffer) of
        [] ->
            {error, {invalid_hl7_msg, Buffer}};
        RawMsg when is_list(RawMsg) ->
            case lists:member(raw, Options) of
                true ->
                    RawMsg;
                false ->
                    decode_msg(RawMsg, [])
            end;
        {error, _Reason} = Error ->
            Error
    end.


-spec encode(Msg :: msg(), [encode_option()]) -> {ok, iolist()} | {error, Reason :: any()}.
encode(Msg, Options) ->
    %% RawMsg =
    case lists:member(raw, Options) of
        true ->
            Msg;
        false ->
            case lists:member(no_simplify, Options) of
                true ->
                    encode_msg(Msg, []);
                false ->
                    encode_and_simplify_msg(Msg, [])
            end
    end. %%,
    %% raw_encode(RawMsg).


-spec segment(segment_id(), raw_msg()) -> raw_segment() | undefined.
segment(SegmentId, Msg) ->
    case lists:keyfind(SegmentId, 1, Msg) of
        Segment when is_tuple(Segment) ->
            Segment;
        false ->
             undefined
    end.


-spec segment(segment_id(), raw_msg(), Repetition :: non_neg_integer()) -> raw_segment() | undefined.
segment(SegmentId, [Segment | Tail], Repetition) ->
    case element(1, Segment) of
        SegmentId when Repetition =:= 1 ->
            Segment;
        SegmentId ->
            segment(SegmentId, Tail, Repetition - 1);
        _Other ->
            segment(SegmentId, Tail, Repetition)
    end;
segment(_SegmentId, [], _Repetition) ->
    undefined.

-spec segment_count(raw_segment_id() | segment_id(), raw_msg() | msg()) -> non_neg_integer().
segment_count(SegmentId, Msg) ->
    segment_count(SegmentId, Msg, 0).

segment_count(SegmentId, [Segment | Tail], Count) ->
    NewCount = case element(1, Segment) of
                   SegmentId ->
                       Count + 1;
                   _Other ->
                       Count
               end,
    segment_count(SegmentId, Tail, NewCount);
segment_count(_SegmentId, [], Count) ->
    Count.


-spec field(field_index(), field_data_type(), field_length(), raw_segment()) -> field() | undefined.
field(Index, DataType, Length, Segment) ->
    ehl7_field:get_field(Index, DataType, Length, Segment).


-spec field(field_index(), raw_segment()) -> raw_field() | undefined.
field(Index, Segment) ->
    ehl7_field:get_field(Index, Segment).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec raw_decode(Buffer :: binary()) -> {ok, raw_msg()} | {error, Reason :: any()}.
raw_decode(_Buffer) ->
    nif_error(?LINE).


-spec raw_encode(Msg :: raw_msg()) -> {ok, iolist()} | {error, Reason :: any()}.
raw_encode(_Msg) ->
    nif_error(?LINE).


nif_error(Line) ->
    error({nif_library_not_loaded, [{module, ?MODULE}, {line, Line}]}).


-spec decode_msg(raw_msg(), Acc :: list()) -> msg().
decode_msg([RawSegment | Tail], Acc) ->
    case ehl7_segment:decode(RawSegment) of
        {error, _Reason} = Error ->
            Error;
        Segment ->
            decode_msg(Tail, [Segment | Acc])
    end;
decode_msg([], Acc) ->
    lists:reverse(Acc).

-spec encode_msg(msg(), Acc :: list()) -> raw_msg().
encode_msg([Segment | Tail], Acc) ->
    encode_msg(Tail, [ehl7_segment:encode(Segment, [tuple]) | Acc]);
encode_msg([], Acc) ->
    lists:reverse(Acc).

-spec encode_and_simplify_msg(msg(), Acc :: list()) -> raw_msg().
encode_and_simplify_msg([Segment | Tail], Acc) ->
    encode_and_simplify_msg(Tail, [simplify(ehl7_segment:encode(Segment, [list])) | Acc]);
encode_and_simplify_msg([], Acc) ->
    lists:reverse(Acc).


-spec simplify(raw_segment()) -> raw_segment().
simplify(RawSegment) ->
    simplify(lists:reverse(RawSegment), []).

simplify([Element | Tail], Acc) when is_list(Element) ->
    simplify(Tail, [simplify_element(remove_last_empty_elements(lists:reverse(Element))) | Acc]);
simplify([Element | Tail], Acc) ->
    simplify(Tail, [Element | Acc]);
simplify([], [Element]) when not is_list(Element), not is_tuple(Element) ->
    Element;
simplify([], Acc) ->
    list_to_tuple(Acc).


simplify_element([Element]) when not is_list(Element) ->
    Element;
simplify_element([]) ->
    undefined;
simplify_element({Element}) when not is_tuple(Element) ->
    Element;
simplify_element({}) ->
    undefined;
simplify_element(Element) when is_list(Element) ->
    simplify(Element, []);
simplify_element(Element) ->
    Element.


remove_last_empty_elements([Element | Tail]) when Element =:= undefined; Element =:= <<>> ->
    remove_last_empty_elements(Tail);
remove_last_empty_elements(Elements) ->
    Elements.
