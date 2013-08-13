%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc Tests for the ehl7 parser.
%%% @end
%%%-------------------------------------------------------------------
-module(ehl7_test).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([msh/0, msa/3, err/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ehl7/include/ehl7_segment.hrl").

-define(FIELD_SEPARATOR, "|").
-define(ENCODING_CHARACTERS, "^~\\&").


segment_test() ->
    Msg1 = [
            msh(),
            msa(<<"20030127203642">>, <<"M001">>, <<"ERROR EN BASE DE DATOS">>),
            err(<<"M001">>, <<"ERROR TECNICO">>)
           ],
    Buf = ehl7:encode(Msg1),
    Msg2 = ehl7:decode(Buf),
    ?assertEqual(Msg1, Msg2).


parser_test() ->
    Buf1 = <<"MSH|^~\\&|SERV|223344^^II|POSM|CARRIER^CL9999^IP|20030127202538||RPA^I08|5307938|P|2.3|||NE|NE\r"
             "MSA|AA|CL999920030127203647||||B006^\r"
             "AUT|TESTPLAN|223344^^II||||5307938||0|0\r"
             "PRD|RT|NOMBRE PRESTADOR SALUD|||||99999999999^CU^GUARDIA\r"
             "PRD|RP||||||9^^N\r"
             "PID|||2233441000013527101=0000000000002|1|APELLIDO^NOMBRE\r"
             "PR1|1||420101^CONSULTA EN CONSULTORIO^NA^||20030127203642|Z\r"
             "AUT|PLANSALUD|||20030127|20030127|5307938|0.00^$|1|1\r"
             "NTE|1||SIN CARGO\r"
             "NTE|2||IVA: SI\r">>,
    Msg1 = ehl7:decode(Buf1),
    PID = ehl7:segment(pid, Msg1),
    ?assertMatch(<<"APELLIDO">>, PID#pid.last_name),
    ?assertMatch(<<"NOMBRE">>, PID#pid.first_name),
    AUT = ehl7:segment(aut, Msg1, 2),
    ?assertMatch(<<"PLANSALUD">>, AUT#aut.plan_id),
    %% We don't trim the redundant trailing elements to avoid mismatches when
    %% comparing the buffers.
    Buf2 = ehl7:encode(Msg1, [{simplify, false}]),
    Msg2 = ehl7:decode(Buf2),
    ?assertEqual(Msg1, Msg2).


msh() ->
    #msh{
       field_separator = <<?FIELD_SEPARATOR>>,
       encoding_characters = <<?ENCODING_CHARACTERS>>,
       %% Sender: server
       sending_application_id = <<"SERV">>,
       sending_facility_id = <<"HMO">>,
       sending_facility_universal_id = <<"223344">>,
       sending_facility_universal_id_type = <<"IIN">>,
       %% Receiver: client
       receiving_application_id = <<"TIPO0100M">>,
       receiving_facility_id = <<"TIPO00000001">>,
       receiving_facility_universal_id = <<"99999999999">>,
       receiving_facility_universal_id_type = <<"CARRIER">>,
       %% Message time
       message_date = calendar:now_to_datetime(os:timestamp()),
       %% Message type
       message_type = <<"ACK">>,
       %% Message control ID
       message_control_id = <<"CL999920030127203647">>,
       %% Processing ID
       processing_id = <<"P">>,
       version = <<"2.4">>,
       accept_ack_type = <<"NE">>,
       application_ack_type = <<"AL">>,
       country_code = <<"USA">>
      }.


msa(MsgControlId, ErrorCode, ErrorText) ->
    #msa{
       ack_code = <<"AE">>,
       message_control_id = MsgControlId,
       error_code = ErrorCode,
       error_text = ErrorText
      }.


err(ErrorCode, ErrorText) ->
    #err{
       error_code =  ErrorCode,
       error_text = ErrorText
      }.
