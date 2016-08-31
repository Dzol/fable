%% ===================================================================
%% @copyright 2016 Joseph Yiasemides
%% @author <joseph.yiasemides@erlang-solutions.com>
%% @end
%% ===================================================================

-module(fable_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%% -------------------------------------------------------------------
%% Suite description
%% -------------------------------------------------------------------

all() ->

    [ {group, positive}
    ].

groups() ->

    [ {positive, [], integration() ++ forest() ++ scan()}
    ].

integration() ->
    [ integration
    , example
    ].

forest() ->
    [ tree
    , forest
    ].

scan() ->

    [ none
    , sentinal
    , spaced
    , operator
    , integer
    , symbol
    , symbols
    , nest
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

none(_) ->
    [] = fable:scan("").

sentinal(_) ->
    [open, close] = fable:scan("()").

spaced(_) ->
    [open, close] = fable:scan("( )").

operator(_) ->
    [open, [$!], 1024, 512, close] = fable:scan("(! 1024 512)").

integer(_) ->
    [open, 1024, close] = fable:scan("(1024)").

symbol(_) ->
    [open, "foo", close] = fable:scan("(foo)").

symbols(_) ->
    [open, "foo", "bar", "baz", close] = fable:scan("(foo bar baz)").

nest(_) ->
    [open,
     "foo",
      open, [$!], 1024, close,
      open, "baz", close,
     close] = fable:scan("(foo (! 1024) (baz))").
