%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc Module that parses and generates HL7 messages.
%%% @end
%%%-------------------------------------------------------------------
-module(ehl7).
-author('Juan Jose Comellas <juanjo@comellas.org>').

%% API
-export([decode/1, decode/2, encode/1, encode/2]).
-export([segment/2, segment/3, segment_count/2]).
-export([field/2, field/4, repetition/2, repetition/4, component/2, component/4, subcomponent/2, subcomponent/4]).
-export([dump/1]).

-export_type([raw_msg/0, msg/0, raw_segment_id/0, segment_id/0, raw_segment/0, segment/0,
              raw_field/0, field/0, field_index/0, field_data_type/0, field_length/0]).

-on_load(init/0).

-define(APP, ehl7).

-type field_index()                             :: non_neg_integer() | [non_neg_integer()].
-type field_data_type()                         :: string | integer | date | float.
-type field_length()                            :: non_neg_integer().

-type field()                                   :: binary() | integer() | calendar:date() | calendar:datetime() | float() | undefined.
-type segment_id()                              :: atom().
-type segment()                                 :: tuple(segment_id() | field()).
-type msg()                                     :: [segment()].

-type raw_field()                               :: binary() | tuple() | undefined.
-type raw_segment_id()                          :: binary().
-type raw_segment()                             :: tuple(raw_segment_id() | raw_field()).
-type raw_msg()                                 :: [raw_segment()].

-type buffer()                                  :: binary().

-type decode_option()                           :: {format, raw | record}.
-type encode_option()                           :: {format, raw | record} | {simplify, boolean()}.

-type error()                                   :: {error, Reason :: term()}.


%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok | error().
init() ->
    PrivDir = case code:priv_dir(?APP) of
                  {error, bad_name} -> "priv";
                  PrivDir1          -> PrivDir1
              end,
    erlang:load_nif(filename:join(PrivDir, "ehl7"), 0).


-spec decode(buffer()) -> msg() | error().
decode(Buffer) ->
    decode(Buffer, [{format, record}, {simplify, true}]).

-spec decode(buffer(), [decode_option()]) -> msg() | raw_msg() | error().
decode(Buffer, Options) ->
    case raw_decode(Buffer) of
        [] ->
            {error, {invalid_hl7_message, Buffer}};
        RawMsg when is_list(RawMsg) ->
            case proplists:get_value(format, Options, record) of
                raw  ->
                    RawMsg;
                record ->
                    decode_msg(RawMsg, [])
            end;
        {error, _Reason} = Error ->
            Error
    end.


-spec encode(msg()) -> buffer() | error().
encode(Msg) ->
    encode(Msg, []).

-spec encode(msg() | raw_msg(), [encode_option()]) -> binary() | error().
encode(Msg, Options) ->
    RawMsg =
        case proplists:get_value(format, Options, record) of
            raw ->
                Msg;
            record ->
                case proplists:get_value(simplify, Options, true) of
                    true ->
                        encode_and_simplify_msg(Msg, []);
                    false ->
                        encode_msg(Msg, [])
                end
        end,
    raw_encode(RawMsg).


-spec segment(segment_id(), raw_msg()) -> raw_segment() | undefined.
segment(SegmentId, Msg) ->
    case lists:keyfind(SegmentId, 1, Msg) of
        Segment when is_tuple(Segment) ->
            Segment;
        false ->
             undefined
    end.


-spec segment(segment_id(), msg() | raw_msg(), Repetition :: non_neg_integer()) -> segment() | raw_segment() | undefined.
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


-spec segment_count(segment_id() | raw_segment_id(), msg() | raw_msg()) -> non_neg_integer().
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
    ehl7_field:field(Index, DataType, Length, Segment).

-spec field(field_index(), raw_segment()) -> raw_field() | undefined.
field(Index, Segment) ->
    ehl7_field:field(Index, Segment).


-spec repetition(field_index(), field_data_type(), field_length(), raw_segment()) -> field() | undefined.
repetition(Index, DataType, Length, Segment) ->
    ehl7_field:repetition(Index, DataType, Length, Segment).

-spec repetition(field_index(), raw_segment()) -> raw_field() | undefined.
repetition(Index, Segment) ->
    ehl7_field:repetition(Index, Segment).


-spec component(field_index(), field_data_type(), field_length(), raw_segment()) -> field() | undefined.
component(Index, DataType, Length, Segment) ->
    ehl7_field:component(Index, DataType, Length, Segment).

-spec component(field_index(), raw_segment()) -> raw_field() | undefined.
component(Index, Segment) ->
    ehl7_field:component(Index, Segment).


-spec subcomponent(field_index(), field_data_type(), field_length(), raw_segment()) -> field() | undefined.
subcomponent(Index, DataType, Length, Segment) ->
    ehl7_field:subcomponent(Index, DataType, Length, Segment).

-spec subcomponent(field_index(), raw_segment()) -> raw_field() | undefined.
subcomponent(Index, Segment) ->
    ehl7_field:subcomponent(Index, Segment).


-spec dump(binary()) -> binary().
dump(Buffer) ->
    dump(Buffer, <<>>).

dump(<<$\r, Tail/binary>>, Acc) ->
    dump(Tail, <<Acc/binary, "\\r\n">>);
dump(<<Char, Tail/binary>>, Acc) ->
    dump(Tail, <<Acc/binary, Char>>);
dump(<<>>, Acc) ->
    Acc.



%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec raw_decode(buffer()) -> raw_msg() | error().
raw_decode(_Buffer) ->
    nif_error(?LINE).


-spec raw_encode(raw_msg()) -> buffer() | error().
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
