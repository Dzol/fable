%% ===================================================================
%% @copyright 2016 Joseph Yiasemides
%% @author <joseph.yiasemides@erlang-solutions.com>
%% @end
%% ===================================================================

-module(parse_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%% -------------------------------------------------------------------
%% Suite description
%% -------------------------------------------------------------------

all() ->

    [ {group, parse}
    ].

groups() ->

    [ {parse, [], parse()}
    ].

parse() ->

    [ tree
    , forest
    ].


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

tree(_) ->
    Tree = [open,
             open, close,
             open, 1024, "foo", close,
             open,
              open,close,
             close,
            close],
    [ [[], [1024, "foo"], [[]]] ] = fable:parse(Tree).

forest(_) ->
    Forest = [open, close,
              open,
               open, close,
              close,
              open, close],
    [ [], [[]], [] ] = fable:parse(Forest).
