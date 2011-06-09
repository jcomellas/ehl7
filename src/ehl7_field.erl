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

-export([get_field/2, get_field/4,
         get_repetition/2, get_repetition/4,
         get_component/2, get_component/4,
         get_subcomponent/2, get_subcomponent/4,
         get_element/2, get_element/4]).


-spec get_field(ehl7:field_index(), ehl7:raw_segment()) -> ehl7:field() | undefined.
get_field(Index, Segment) ->
    get_element(Index, Segment).

-spec get_field(ehl7:field_index(), ehl7:field_data_type(), ehl7:field_length(), ehl7:raw_segment()) -> ehl7:field() | undefined.
get_field(Index, DataType, Length, Segment) ->
    get_element(Index, DataType, Length, Segment).


-spec get_repetition(ehl7:field_index(), ehl7:raw_segment()) -> ehl7:field() | undefined.
get_repetition([_FieldIndex, _RepIndex] = Index, Segment) ->
    get_element(Index, Segment).

-spec get_repetition(ehl7:field_index(), ehl7:field_data_type(), ehl7:field_length(), ehl7:raw_segment()) -> ehl7:field() | undefined.
get_repetition([_FieldIndex, _RepIndex] = Index, DataType, Length, Segment) ->
    get_element(Index, DataType, Length, Segment).


-spec get_component(ehl7:field_index(), ehl7:raw_segment()) -> ehl7:field() | undefined.
get_component([FieldIndex, CompIndex], Segment) ->
    get_element([FieldIndex, 1, CompIndex], Segment);
get_component([_FieldIndex, _RepIndex, _CompIndex] = Index, Segment) ->
    get_element(Index, Segment).

-spec get_component(ehl7:field_index(), ehl7:field_data_type(), ehl7:field_length(), ehl7:raw_segment()) -> ehl7:field() | undefined.
get_component([FieldIndex, CompIndex], DataType, Length, Segment) ->
    get_element([FieldIndex, 1, CompIndex], DataType, Length, Segment);
get_component([_FieldIndex, _RepIndex, _CompIndex] = Index, DataType, Length, Segment) ->
    get_element(Index, DataType, Length, Segment).


-spec get_subcomponent(ehl7:field_index(), ehl7:raw_segment()) -> ehl7:field() | undefined.
get_subcomponent([FieldIndex, CompIndex, SubcompIndex], Segment) ->
    get_element([FieldIndex, 1, CompIndex, SubcompIndex], Segment);
get_subcomponent([_FieldIndex, _RepIndex, _CompIndex, _SubcompIndex] = Index, Segment) ->
    get_element(Index, Segment).

-spec get_subcomponent(ehl7:field_index(), ehl7:field_data_type(), ehl7:field_length(), ehl7:raw_segment()) -> ehl7:field() | undefined.
get_subcomponent([FieldIndex, CompIndex, SubcompIndex], DataType, Length, Segment) ->
    get_element([FieldIndex, 1, CompIndex, SubcompIndex], DataType, Length, Segment);
get_subcomponent([_FieldIndex, _RepIndex, _CompIndex, _SubcompIndex] = Index, DataType, Length, Segment) ->
    get_element(Index, DataType, Length, Segment).


-spec get_element(ehl7:field_index(), ehl7:field_data_type(), ehl7:field_length(), ehl7:raw_segment()) -> ehl7:field() | undefined.
get_element(Index, DataType, Length, Segment) ->
    case get_element(Index, Segment) of
        Tuple when is_tuple(Tuple) ->
            Tuple;
        Value ->
            convert(Value, DataType, Length)
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
    case bstr:member(Value, $.) of
        true ->
            bstr:to_float(Value);
        false ->
            bstr:to_integer(Value) * 1.0
    end.

