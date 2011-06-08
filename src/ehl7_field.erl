%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <jcomellas@erlar.com>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc Module that parses and generates fields for HL7 messages.
%%% @end
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(ehl7_field).
-author('Juan Jose Comellas <jcomellas@erlar.com>').

-export([get_field/2, get_field/4]).


-spec get_field(ehl7:field_index(), ehl7:field_data_type(), ehl7:field_length(), ehl7:raw_segment()) -> ehl7:field() | undefined.
get_field(Index, DataType, Length, Segment) ->
    case get_field(Index, Segment) of
        Tuple when is_tuple(Tuple) ->
            Tuple;
        Value ->
            convert(Value, DataType, Length)
    end.


-spec get_field(ehl7:field_index(), ehl7:raw_segment()) -> ehl7:raw_field() | undefined.
get_field([Index | Tail], Segment) ->
    get_element([Index + 1 | Tail], Segment);
get_field(Index, Segment) when is_integer(Index)->
    get_element([Index + 1], Segment).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_element(ehl7:field_index(), tuple()) -> ehl7:raw_field() | undefined.
get_element([Index | Tail], Element) when is_tuple(Element) ->
    if
        tuple_size(Element) >= Index ->
            get_element(Tail, element(Index, Element));
        true ->
            undefined
    end;
get_element([Index | Tail], Element) when not is_tuple(Element) ->
    case Index of
        1 ->
            get_element(Tail, Element);
        _Other ->
            undefined
    end;
get_element([], Element) ->
    Element.


-spec convert(ehl7:raw_field() | undefined, ehl7:field_data_type(), ehl7:field_length()) -> ehl7:field().
convert(undefined, _DataType, _Length) ->
    undefined;
convert(Value, string, _Length) ->
    Value;
convert(<<>>, _DataType, _Length) ->
    undefined;
convert(Value, integer, _Length) ->
    bstr:to_integer(Value);
convert(<<Year:4/binary, Month:2/binary, Day:2/binary, _Tail/binary>>, date, 8) ->
    {bstr:to_integer(Year), bstr:to_integer(Month), bstr:to_integer(Day)};
convert(<<Year:4/binary, Month:2/binary, Day:2/binary, Hour:2/binary, Min:2/binary, _Tail/binary>>, date, 12) ->
    {{bstr:to_integer(Year), bstr:to_integer(Month), bstr:to_integer(Day)},
     {bstr:to_integer(Hour), bstr:to_integer(Min), 0}};
convert(<<Year:4/binary, Month:2/binary, Day:2/binary, Hour:2/binary, Min:2/binary, Sec:2/binary, _Tail/binary>>, date, 14) ->
    {{bstr:to_integer(Year), bstr:to_integer(Month), bstr:to_integer(Day)},
     {bstr:to_integer(Hour), bstr:to_integer(Min), bstr:to_integer(Sec)}};
convert(Value, float, _Length) ->
    bstr:to_float(Value).

