%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <jcomellas@erlar.com>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc Module that parses and generates segments for HL7 messages.
%%% @end
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(ehl7_segment).
-author('Juan Jose Comellas <jcomellas@erlar.com>').

-include("include/ehl7_segment.hrl").

-import(ehl7, [get_field/4]).
-export([decode/1]).


%% Decode a segment encoded as a tuple and convert it to a record
decode(Segment) ->
    SegmentId = binary_to_atom(bstr:lower(element(1, Segment)), latin1),
    decode(SegmentId, Segment).


%% Decode the AUT (Authorization information) segment
decode(aut, Segment) ->
    #aut{
        plan_id = get_field([1,1], string, 10, Segment),
        plan_name = get_field([1,2], string, 20, Segment),
        company_id = get_field([2,1], string, 6, Segment),
        company_name = get_field([2,2], string, 30, Segment),
        company_id_coding_system = get_field([2,3], string, 20, Segment),
        start_date = get_field(4, date, 8, Segment),
        end_date = get_field(5, date, 8, Segment),
        authorization_id = get_field([6,1], string, 20, Segment),
        requested_treatments = get_field(8, integer, 2, Segment),
        authorized_treatments = get_field(9, integer, 2, Segment)
       };
%% Decode the DG1 (Diagnosis information) segment
decode(dg1, Segment) ->
    #dg1{
        set_id = get_field(1, integer, 4, Segment),
        diagnosis_id = get_field([3,1], string, 20, Segment),
        name = get_field([3,2], string, 32, Segment),
        coding_system = get_field([3,3], string, 10, Segment),
        diagnosis_type = get_field(6, string, 2, Segment)
       };
%% Decode the DSC (Continuation pointer) segment
decode(dsc, Segment) ->
    #dsc{
        continuation_pointer = get_field(1, string, 15, Segment)
       };
%% Decode the DSP (Display data) segment
decode(dsp, Segment) ->
    #dsp{
        set_id = get_field(1, integer, 4, Segment),
        display_level = get_field(2, string, 4, Segment),
        data_line = get_field(3, string, 40, Segment),
        break_point = get_field(4, string, 2, Segment),
        result_id = get_field(5, string, 20, Segment)
       };
%% Decode the ERR (Error information) segment
decode(err, Segment) ->
    #err{
        segment_id = get_field([1,1], string, 3, Segment),
        sequence = get_field([1,2], integer, 3, Segment),
        field_pos = get_field([1,3], integer, 3, Segment),
        error_code = get_field([1,4,1], string, 9, Segment),
        error_text = get_field([1,4,2], string, 61, Segment)
       };
%% Decode the EVN (Event type) segment
decode(evn, Segment) ->
    #evn{
        recorded_date = get_field(2, date, 14, Segment),
        planned_event_date = get_field(3, date, 14, Segment)
       };
%% Decode the IN1 (Insurance) segment
decode(in1, Segment) ->
    #in1{
        set_id = get_field(1, integer, 4, Segment),
        plan_id = get_field([2,1], string, 20, Segment),
        plan_name = get_field([2,2], string, 30, Segment),
        company_id = get_field([3,1], string, 6, Segment),
        company_assigning_authority_id = get_field([3,4,1], string, 10, Segment),
        company_id_type = get_field([3,4,5], string, 10, Segment),
        authorization_number = get_field([14,1], string, 20, Segment),
        auhtorization_date = get_field([14,2], date, 8, Segment)
       };
%% Decode the MSA (Message acknowledgment) segment
decode(msa, Segment) ->
    #msa{
        ack_code = get_field(1, string, 2, Segment),
        message_control_id = get_field(2, string, 20, Segment),
        error_code = get_field([6,1], string, 10, Segment),
        error_text = get_field([6,2], string, 40, Segment)
       };
%% Decode the MSH (Message header) segment
decode(msh, Segment) ->
    #msh{
        field_separator = get_field(1, string, 1, Segment),
        encoding_characters = get_field(2, string, 4, Segment),
        sending_application_id = get_field([3,1], string, 12, Segment),
        sending_facility_id = get_field([4,1], string, 12, Segment),
        sending_facility_universal_id = get_field([4,2], string, 20, Segment),
        sending_facility_universal_id_type = get_field([4,3], string, 20, Segment),
        receiving_application_id = get_field([5,1], string, 12, Segment),
        receiving_facility_id = get_field([6,1], string, 12, Segment),
        receiving_facility_universal_id = get_field([6,2], string, 20, Segment),
        receiving_facility_universal_id_type = get_field([6,3], string, 20, Segment),
        message_date = get_field(7, date, 14, Segment),
        message_type = get_field([9,1], string, 3, Segment),
        trigger_event = get_field([9,2], string, 3, Segment),
        message_structure = get_field([9,3], string, 7, Segment),
        message_control_id = get_field(10, string, 20, Segment),
        processing_id = get_field([11], string, 3, Segment),
        version = get_field([12], string, 8, Segment),
        accept_ack_type = get_field([15], string, 2, Segment),
        application_ack_type = get_field([16], string, 2, Segment),
        country_code = get_field([17], string, 3, Segment)
       };
%% Decode the NTE (Notes and comments) segment
decode(nte, Segment) ->
    #nte{
        set_id = get_field(1, integer, 4, Segment),
        comment = get_field(3, string, 512, Segment)
       };
%% Decode the PID (Patient information) segment
decode(pid, Segment) ->
    #pid{
        set_id = get_field(1, integer, 4, Segment),
        patient_id = get_field([3,1,1], string, 20, Segment),
        patient_document_id = get_field([3,1,1], string, 20, Segment),
        assigning_authority_id = get_field([3,4,1], string, 6, Segment),
        assigning_authority_universal_id = get_field([3,4,2], string, 6, Segment),
        assigning_authority_universal_id_type = get_field([3,4,3], string, 10, Segment),
        id_type = get_field([3,5], string, 2, Segment),
        last_name = get_field([5,1], string, 25, Segment),
        first_name = get_field([5,2], string, 25, Segment)
       };
%% Decode the PR1 (Procedure information) segment
decode(pr1, Segment) ->
    #pr1{
        set_id = get_field(1, integer, 4, Segment),
        procedure_id = get_field([3,1], string, 20, Segment),
        procedure_name = get_field([3,2], string, 30, Segment),
        coding_system = get_field([3,3], string, 4, Segment),
        date = get_field(5, date, 14, Segment)
       };
%% Decode the PRD (Provider data) segment
decode(prd, Segment) ->
    #prd{
        role_id = get_field([1,1,1], string, 5, Segment),
        role_name = get_field([1,1,2], string, 30, Segment),
        role_coding_system = get_field([1,1,3], string, 7, Segment),
        specialty_id = get_field([1,2,1], string, 5, Segment),
        specialty_name = get_field([1,2,2], string, 30, Segment),
        specialty_coding_system = get_field([1,2,3], string, 7, Segment),
        last_name = get_field([2,1], string, 40, Segment),
        first_name = get_field([2,2], string, 30, Segment),
        street = get_field([3,1], string, 20, Segment),
        other_designation = get_field([3,2], string, 20, Segment),
        city = get_field([3,3], string, 30, Segment),
        state = get_field([3,4], string, 1, Segment),
        postal_code = get_field([3,5], string, 10, Segment),
        country_code = get_field([3,6], string, 3, Segment),
        address_type = get_field([3,7], string, 1, Segment),
        provider_id = get_field([7,1], string, 15, Segment),
        provider_id_type = get_field([7,2,1], string, 2, Segment),
        provider_id_type_medical = get_field([7,2,2], string, 1, Segment),
        provider_id_type_province = get_field([7,2,3], string, 1, Segment),
        provider_id_alternate_qualifier = get_field([7,3], string, 8, Segment)
       };
%% Decode the PV1 (Patient visit) segment
decode(pv1, Segment) ->
    #pv1{
        set_id = get_field(1, string, 4, Segment),
        patient_class = get_field(2, string, 1, Segment),
        patient_point_of_care = get_field([3,1], string, 10, Segment),
        patient_location_facility = get_field([3,4], string, 21, Segment),
        admission_type = get_field(4, string, 34, Segment),
        attending_doctor_id = get_field([7,1], string, 20, Segment),
        attending_doctor_last_name = get_field([7,2], string, 25, Segment),
        attending_doctor_first_name = get_field([7,3], string, 25, Segment),
        attending_doctor_assigning_authority = get_field([7,9], string, 21, Segment),
        referring_doctor_id = get_field([8,1], string, 20, Segment),
        referring_doctor_last_name = get_field([8,2], string, 25, Segment),
        referring_doctor_first_name = get_field([8,3], string, 25, Segment),
        referring_doctor_assigning_authority = get_field([8,9], string, 21, Segment),
        hospital_service = get_field(10, string, 99, Segment),
        readmission_indicator = get_field([13], string, 2, Segment),
        discharge_diposition = get_field([36], string, 3, Segment),
        admit_date = get_field([44], date, 12, Segment),
        discharge_date = get_field([45], date, 12, Segment),
        visit_indicator = get_field([51], string, 1, Segment)
       };
%% Decode the PV2 (Patient visit - additional information) segment
decode(pv2, Segment) ->
    #pv2{
        transfer_reason_id = get_field([4,1], string, 20, Segment)
       };
%% Decode the QAK (Query acknowledgment) segment
decode(qak, Segment) ->
    #qak{
        query_tag = get_field(1, string, 32, Segment),
        query_response_status = get_field(2, string, 4, Segment),
        query_id = get_field([3,1], string, 14, Segment),
        query_name = get_field([3,2], string, 30, Segment)
       };
%% Decode the QPD_Q15 (Query parameter definition -- procedure totals query) segment
decode(qpd_q15, Segment) ->
    #qpd_q15{
        query_id = get_field([1,1], string, 20, Segment),
        query_name = get_field([1,2], string, 30, Segment),
        query_tag = get_field(2, string, 32, Segment),
        provider_id = get_field([3,1], string, 15, Segment),
        provider_id_type = get_field([3,2], string, 4, Segment),
        start_date = get_field(4, date, 12, Segment),
        end_date = get_field(5, date, 12, Segment),
        procedure_id = get_field([6,1], string, 30, Segment),
        procedure_coding_system = get_field([6,2], string, 8, Segment),
        authorizer_id = get_field([7,1], string, 6, Segment)
       };
%% Decode the RCP (Response control parameter) segment
decode(rcp, Segment) ->
    #rcp{
        query_priority = get_field(1, string, 1, Segment),
        response_limit = get_field([2,1], integer, 10, Segment),
        response_unit = get_field([2,2,1], string, 2, Segment),
        response_modality_id = get_field([3,1], string, 10, Segment),
        execution_date = get_field(4, date, 12, Segment),
        sort_by = get_field(6, string, 512, Segment)
       };
%% Decode the RF1 (Referral information) segment
decode(rf1, Segment) ->
    #rf1{
        referral_status_id = get_field([1,1], string, 5, Segment),
        referral_status_description = get_field([1,2], string, 15, Segment),
        referral_type_id = get_field([3,1], string, 5, Segment),
        referral_type_description = get_field([3,2], string, 15, Segment),
        originating_referral_id = get_field([6,1], string, 15, Segment),
        effective_date = get_field(7, date, 12, Segment),
        expiration_date = get_field(8, date, 12, Segment),
        process_date = get_field(9, date, 12, Segment),
        referral_reason_id = get_field([10,1], string, 21, Segment)
       };
%% Decode the ZAU (Procedure authorization information) segment
decode(zau, Segment) ->
    #zau{
        prev_authorization_id = get_field([1,1], string, 15, Segment),
        payor_control_id = get_field([2,1], string, 15, Segment),
        authorization_status = get_field([3,1], string, 4, Segment),
        authorization_status_text = get_field([3,2], string, 15, Segment),
        pre_authorization_id = get_field([4,1], string, 15, Segment),
        pre_authorization_date = get_field(5, string, 8, Segment),
        copay = get_field([6,1,1], string, 10, Segment),
        copay_currency = get_field([6,1,2], string, 10, Segment)
       };
%% Decode the ZIN (Additional insurance information) segment
decode(zin, Segment) ->
    #zin{
        eligibility_indicator = get_field(1, string, 1, Segment),
        patient_vat_status = get_field([2,1], string, 4, Segment),
        patient_vat_status_text = get_field([2,2], string, 7, Segment)
       };
decode(_SegmentId, Segment) ->
    {error, {unknown_segment_id, element(1, Segment)}}.
