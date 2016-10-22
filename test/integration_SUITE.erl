%% ===================================================================
%% @copyright 2016 Joseph Yiasemides
%% @author <joseph.yiasemides@erlang-solutions.com>
%% @end
%% ===================================================================

-module(integration_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%% -------------------------------------------------------------------
%% Suite description
%% -------------------------------------------------------------------

all() ->

    [ {group, integration}
    ].

groups() ->

    [ {integration, [], integration()}
    ].

integration() ->

    [ integration
    , example
    ].

%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

integration(_) ->
    Tree = fable:scan("(() (+ 1024 foo) (()))"),
    [ [[], ["+", 1024, "foo"], [[]]] ] = fable:parse(Tree).

example(_) ->
    Tree = fable:scan("(first (list 1 (+ 2 3) 9))"),
    [ ["first", ["list", 1, ["+", 2, 3], 9]] ] = fable:parse(Tree).
