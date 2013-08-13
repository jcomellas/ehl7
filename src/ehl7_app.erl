%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc Erlang HL7 Parser application.
%%% @end
%%%-------------------------------------------------------------------
-module(ehl7_app).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ehl7_sup:start_link().

stop(_State) ->
    ok.
