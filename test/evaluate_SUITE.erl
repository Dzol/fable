%% ===================================================================
%% @copyright 2016 Joseph Yiasemides
%% @author <joseph.yiasemides@erlang-solutions.com>
%% @end
%% ===================================================================

-module(evaluate_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%% -------------------------------------------------------------------
%% Suite description
%% -------------------------------------------------------------------

all() ->

    [ {group, evaluate}
    ].

groups() ->

    [ {evaluate, [], evaluate()}
    ].

evaluate() ->

    [ simple
    , quote
    ].


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

simple(_) ->
    Tree = [ "+"
           , ["*", 1, 2]
           , 3
           , ["*", 16, 1]
           ],
    21 = fable:evaluate(Tree).

quote(_) ->
    Q = ["+", 2, 3],
    T = ["quote", Q],
    Q = fable:evaluate(T),
    5 = fable:evaluate(Q).
