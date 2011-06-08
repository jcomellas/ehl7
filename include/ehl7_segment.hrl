%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <jcomellas@erlar.com>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc Module that parses and generates segments for HL7 messages.
%%% @end
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------

%% AUT: Authorization information
-record(aut, {
          plan_id :: binary(),
          plan_name :: binary(),
          company_id :: binary(),
          company_name :: binary(),
          company_id_coding_system :: binary(),
          start_date :: calendar:date(),
          end_date :: calendar:date(),
          authorization_id :: binary(),
          requested_treatments :: integer(),
          authorized_treatments :: integer()
         }).

%% DG1: Diagnosis information
-record(dg1, {
          set_id :: integer(),
          diagnosis_id :: binary(),
          name :: binary(),
          coding_system :: binary(),
          diagnosis_type :: binary()
         }).

%% DSC: Continuation pointer
-record(dsc, {
          continuation_pointer :: binary()
         }).

%% DSP: Display data
-record(dsp, {
          set_id :: integer(),
          display_level :: binary(),
          data_line :: binary(),
          break_point :: binary(),
          result_id :: binary()
         }).

%% ERR: Error information
-record(err, {
          segment_id :: binary(),
          sequence :: integer(),
          field_pos :: integer(),
          error_code :: binary(),
          error_text :: binary()
         }).

%% EVN: Event type
-record(evn, {
          recorded_date :: calendar:datetime(),
          planned_event_date :: calendar:datetime()
         }).

%% IN1: Insurance
-record(in1, {
          set_id :: integer(),
          plan_id :: binary(),
          plan_name :: binary(),
          company_id :: binary(),
          company_assigning_authority_id :: binary(),
          company_id_type :: binary(),
          authorization_number :: binary(),
          auhtorization_date :: calendar:date()
         }).

%% MSA: Message acknowledgment
-record(msa, {
          ack_code :: binary(),
          message_control_id :: binary(),
          error_code :: binary(),
          error_text :: binary()
         }).

%% MSH: Message header
-record(msh, {
          field_separator :: binary(),
          encoding_characters :: binary(),
          sending_application_id :: binary(),
          sending_facility_id :: binary(),
          sending_facility_universal_id :: binary(),
          sending_facility_universal_id_type :: binary(),
          receiving_application_id :: binary(),
          receiving_facility_id :: binary(),
          receiving_facility_universal_id :: binary(),
          receiving_facility_universal_id_type :: binary(),
          message_date :: calendar:datetime(),
          message_type :: binary(),
          trigger_event :: binary(),
          message_structure :: binary(),
          message_control_id :: binary(),
          processing_id :: binary(),
          version :: binary(),
          accept_ack_type :: binary(),
          application_ack_type :: binary(),
          country_code :: binary()
         }).

%% NTE: Notes and comments
-record(nte, {
          set_id :: integer(),
          comment :: binary()
         }).

%% PID: Patient information
-record(pid, {
          set_id :: integer(),
          patient_id :: binary(),
          patient_document_id :: binary(),
          assigning_authority_id :: binary(),
          assigning_authority_universal_id :: binary(),
          assigning_authority_universal_id_type :: binary(),
          id_type :: binary(),
          last_name :: binary(),
          first_name :: binary()
         }).

%% PR1: Procedure information
-record(pr1, {
          set_id :: integer(),
          procedure_id :: binary(),
          procedure_name :: binary(),
          coding_system :: binary(),
          date :: calendar:datetime()
         }).

%% PRD: Provider data
-record(prd, {
          role_id :: binary(),
          role_name :: binary(),
          role_coding_system :: binary(),
          specialty_id :: binary(),
          specialty_name :: binary(),
          specialty_coding_system :: binary(),
          last_name :: binary(),
          first_name :: binary(),
          street :: binary(),
          other_designation :: binary(),
          city :: binary(),
          state :: binary(),
          postal_code :: binary(),
          country_code :: binary(),
          address_type :: binary(),
          provider_id :: binary(),
          provider_id_type :: binary(),
          provider_id_type_medical :: binary(),
          provider_id_type_province :: binary(),
          provider_id_alternate_qualifier :: binary()
         }).

%% PV1: Patient visit
-record(pv1, {
          set_id :: binary(),
          patient_class :: binary(),
          patient_point_of_care :: binary(),
          patient_location_facility :: binary(),
          admission_type :: binary(),
          attending_doctor_id :: binary(),
          attending_doctor_last_name :: binary(),
          attending_doctor_first_name :: binary(),
          attending_doctor_assigning_authority :: binary(),
          referring_doctor_id :: binary(),
          referring_doctor_last_name :: binary(),
          referring_doctor_first_name :: binary(),
          referring_doctor_assigning_authority :: binary(),
          hospital_service :: binary(),
          readmission_indicator :: binary(),
          discharge_diposition :: binary(),
          admit_date :: calendar:datetime(),
          discharge_date :: calendar:datetime(),
          visit_indicator :: binary()
         }).

%% PV2: Patient visit - additional information
-record(pv2, {
          transfer_reason_id :: binary()
         }).

%% QAK: Query acknowledgment
-record(qak, {
          query_tag :: binary(),
          query_response_status :: binary(),
          query_id :: binary(),
          query_name :: binary()
         }).

%% QPD_Q15: Query parameter definition -- procedure totals query
-record(qpd_q15, {
          query_id :: binary(),
          query_name :: binary(),
          query_tag :: binary(),
          provider_id :: binary(),
          provider_id_type :: binary(),
          start_date :: calendar:datetime(),
          end_date :: calendar:datetime(),
          procedure_id :: binary(),
          procedure_coding_system :: binary(),
          authorizer_id :: binary()
         }).

%% RCP: Response control parameter
-record(rcp, {
          query_priority :: binary(),
          response_limit :: integer(),
          response_unit :: binary(),
          response_modality_id :: binary(),
          execution_date :: calendar:datetime(),
          sort_by :: binary()
         }).

%% RF1: Referral information
-record(rf1, {
          referral_status_id :: binary(),
          referral_status_description :: binary(),
          referral_type_id :: binary(),
          referral_type_description :: binary(),
          originating_referral_id :: binary(),
          effective_date :: calendar:datetime(),
          expiration_date :: calendar:datetime(),
          process_date :: calendar:datetime(),
          referral_reason_id :: binary()
         }).

%% ZAU: Procedure authorization information
-record(zau, {
          prev_authorization_id :: binary(),
          payor_control_id :: binary(),
          authorization_status :: binary(),
          authorization_status_text :: binary(),
          pre_authorization_id :: binary(),
          pre_authorization_date :: binary(),
          copay :: binary(),
          copay_currency :: binary()
         }).

%% ZIN: Additional insurance information
-record(zin, {
          eligibility_indicator :: binary(),
          patient_vat_status :: binary(),
          patient_vat_status_text :: binary()
         }).

