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
-export([decode/2, encode/2]).
-export([get_segment/2, get_field/2, get_field/4]).

-export_type([raw_msg/0, segment_id/0, raw_segment/0, raw_field/0, field/0,
              field_index/0, field_data_type/0, field_length/0]).

-on_load(init/0).

-type raw_msg() :: [tuple()].
-type msg() :: [tuple()].
-type segment_id() :: binary().
-type raw_segment() :: tuple().
-type raw_field() :: binary().
-type field() :: tuple() | binary() | integer() | calendar:date() | calendar:datetime() | float().
-type field_index() :: non_neg_integer() | [non_neg_integer()].
-type field_data_type() :: 'string' | 'integer' | 'date' | 'float'.
-type field_length() :: non_neg_integer().
-type decode_option() :: raw.
-type encode_option() :: raw.


%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok | {error, Reason :: any()}.
init() ->
    erlang:load_nif("priv/ehl7", 0).


-spec decode(Buffer :: binary(), [decode_option()]) -> {ok, raw_msg()} | {error, Reason :: any()}.
decode(Buffer, Options) ->
    case raw_decode(Buffer) of
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
    RawMsg = case lists:members(raw, Options) of
                true ->
                    Msg;
                false ->
                    encode_msg(Msg, [])
            end,
    raw_encode(RawMsg).


-spec get_segment(segment_id(), raw_msg()) -> raw_segment() | undefined.
get_segment(SegmentId, Msg) ->
    case lists:keyfind(SegmentId, 1, Msg) of
        Segment when is_tuple(Segment) ->
            Segment;
        false ->
             undefined
    end.


-spec get_field(field_index(), field_data_type(), field_length(), raw_segment()) -> field() | undefined.
get_field(Index, DataType, Length, Segment) ->
    ehl7_field:get_field(Index, DataType, Length, Segment).


-spec get_field(field_index(), raw_segment()) -> raw_field() | undefined.
get_field(Index, Segment) ->
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
encode_msg(_Msg, _Acc) ->
    throw(not_implemented).
