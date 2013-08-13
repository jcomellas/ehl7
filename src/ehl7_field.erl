%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc Module that parses and generates fields for HL7 messages.
%%% @end
%%%-------------------------------------------------------------------
-module(ehl7_field).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([field/2, field/4,
         repetition/2, repetition/4,
         component/2, component/4,
         subcomponent/2, subcomponent/4,
         get_element/2, get_element/4,
         from_raw_value/3,
         to_raw_value/3]).


-spec field(ehl7:field_index(), ehl7:raw_segment()) -> ehl7:field() | undefined.
field(Index, Segment) ->
    get_element(Index, Segment).

-spec field(ehl7:field_index(), ehl7:field_data_type(), ehl7:field_length(), ehl7:raw_segment()) -> ehl7:field() | undefined.
field(Index, DataType, Length, Segment) ->
    get_element(Index, DataType, Length, Segment).


-spec repetition(ehl7:field_index(), ehl7:raw_segment()) -> ehl7:field() | undefined.
repetition([_FieldIndex, _RepIndex] = Index, Segment) ->
    get_element(Index, Segment).

-spec repetition(ehl7:field_index(), ehl7:field_data_type(), ehl7:field_length(), ehl7:raw_segment()) -> ehl7:field() | undefined.
repetition([_FieldIndex, _RepIndex] = Index, DataType, Length, Segment) ->
    get_element(Index, DataType, Length, Segment).


-spec component(ehl7:field_index(), ehl7:raw_segment()) -> ehl7:field() | undefined.
component([FieldIndex, CompIndex], Segment) ->
    get_element([FieldIndex, 1, CompIndex], Segment);
component([_FieldIndex, _RepIndex, _CompIndex] = Index, Segment) ->
    get_element(Index, Segment).

-spec component(ehl7:field_index(), ehl7:field_data_type(), ehl7:field_length(), ehl7:raw_segment()) -> ehl7:field() | undefined.
component([FieldIndex, CompIndex], DataType, Length, Segment) ->
    get_element([FieldIndex, 1, CompIndex], DataType, Length, Segment);
component([_FieldIndex, _RepIndex, _CompIndex] = Index, DataType, Length, Segment) ->
    get_element(Index, DataType, Length, Segment).


-spec subcomponent(ehl7:field_index(), ehl7:raw_segment()) -> ehl7:field() | undefined.
subcomponent([FieldIndex, CompIndex, SubcompIndex], Segment) ->
    get_element([FieldIndex, 1, CompIndex, SubcompIndex], Segment);
subcomponent([_FieldIndex, _RepIndex, _CompIndex, _SubcompIndex] = Index, Segment) ->
    get_element(Index, Segment).

-spec subcomponent(ehl7:field_index(), ehl7:field_data_type(), ehl7:field_length(), ehl7:raw_segment()) -> ehl7:field() | undefined.
subcomponent([FieldIndex, CompIndex, SubcompIndex], DataType, Length, Segment) ->
    get_element([FieldIndex, 1, CompIndex, SubcompIndex], DataType, Length, Segment);
subcomponent([_FieldIndex, _RepIndex, _CompIndex, _SubcompIndex] = Index, DataType, Length, Segment) ->
    get_element(Index, DataType, Length, Segment).


-spec get_element(ehl7:field_index(), ehl7:field_data_type(), ehl7:field_length(), ehl7:raw_segment()) -> ehl7:field() | undefined.
get_element(Index, DataType, Length, Segment) ->
    case get_element(Index, Segment) of
        Tuple when is_tuple(Tuple) ->
            Tuple;
        Value ->
            from_raw_value(Value, DataType, Length)
    end.

-spec get_element(ehl7:field_index(), ehl7:raw_segment()) -> ehl7:raw_field() | undefined.
get_element([Index | Tail], Segment) ->
    get_subelement([Index + 1 | Tail], Segment);
get_element(Index, Segment) when is_integer(Index)->
    get_subelement([Index + 1], Segment).



%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_subelement(ehl7:field_index(), tuple()) -> ehl7:raw_field() | undefined.
get_subelement([Index | Tail], Element) when is_tuple(Element) ->
    if
        tuple_size(Element) >= Index ->
            get_subelement(Tail, element(Index, Element));
        true ->
            undefined
    end;
get_subelement([Index | Tail], Element) when not is_tuple(Element) ->
    case Index of
        1 ->
            get_subelement(Tail, Element);
        _Other ->
            undefined
    end;
get_subelement([], Element) ->
    Element.


-spec from_raw_value(ehl7:raw_field() | undefined, ehl7:field_data_type(), ehl7:field_length()) -> ehl7:field().
from_raw_value(undefined, _DataType, _Length) ->
    undefined;
from_raw_value(Value, string, _Length) ->
    Value;
from_raw_value(<<>>, _DataType, _Length) ->
    undefined;
from_raw_value(Value, integer, _Length) ->
    binary_to_integer(Value);
from_raw_value(<<Year:4/binary, Month:2/binary, Day:2/binary, _Tail/binary>>, date, 8) ->
    {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)};
from_raw_value(<<Year:4/binary, Month:2/binary, Day:2/binary, Hour:2/binary, Min:2/binary, _Tail/binary>>, date, 12) ->
    {{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)},
     {binary_to_integer(Hour), binary_to_integer(Min), 0}};
from_raw_value(<<Year:4/binary, Month:2/binary, Day:2/binary, Hour:2/binary, Min:2/binary, Sec:2/binary, _Tail/binary>>, date, 14) ->
    {{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)},
     {binary_to_integer(Hour), binary_to_integer(Min), binary_to_integer(Sec)}};
from_raw_value(Value, float, _Length) ->
    case bstr:member(Value, $.) of
        true ->
            binary_to_float(Value);
        false ->
            float(binary_to_integer(Value))
    end.


-spec to_raw_value(ehl7:field() | undefined, ehl7:field_data_type(), ehl7:field_length()) -> ehl7:raw_field().
to_raw_value(Value, _DataType, _Length) when Value =:= undefined; Value =:= <<>> ->
    Value;
to_raw_value(Value, string, _Length) ->
    Value;
to_raw_value(Value, integer, _Length) ->
    integer_to_binary(Value);
to_raw_value({Year, Month, Day}, date, 8) ->
    YearStr = bstr:lpad(integer_to_binary(Year), 4, $0),
    MonthStr = bstr:lpad(integer_to_binary(Month), 2, $0),
    DayStr = bstr:lpad(integer_to_binary(Day), 2, $0),
    <<YearStr:4/binary, MonthStr:2/binary, DayStr:2/binary>>;
to_raw_value({{Year, Month, Day}, {Hour, Min, _Sec}}, date, 12) ->
    YearStr = bstr:lpad(integer_to_binary(Year), 4, $0),
    MonthStr = bstr:lpad(integer_to_binary(Month), 2, $0),
    DayStr = bstr:lpad(integer_to_binary(Day), 2, $0),
    HourStr = bstr:lpad(integer_to_binary(Hour), 2, $0),
    MinStr = bstr:lpad(integer_to_binary(Min), 2, $0),
    <<YearStr:4/binary, MonthStr:2/binary, DayStr:2/binary, HourStr:2/binary, MinStr:2/binary>>;
to_raw_value({{Year, Month, Day}, {Hour, Min, Sec}}, date, 14) ->
    YearStr = bstr:lpad(integer_to_binary(Year), 4, $0),
    MonthStr = bstr:lpad(integer_to_binary(Month), 2, $0),
    DayStr = bstr:lpad(integer_to_binary(Day), 2, $0),
    HourStr = bstr:lpad(integer_to_binary(Hour), 2, $0),
    MinStr = bstr:lpad(integer_to_binary(Min), 2, $0),
    SecStr = bstr:lpad(integer_to_binary(Sec), 2, $0),
    <<YearStr:4/binary, MonthStr:2/binary, DayStr:2/binary, HourStr:2/binary, MinStr:2/binary, SecStr:2/binary>>;
to_raw_value(Value, float, _Length) ->
    bstr:from_float(Value).
