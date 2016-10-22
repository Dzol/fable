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

    [ {group, integration}
    , {group, evaluate}
    , {group, parse}
    , {group, scan}
    ].

groups() ->

    [ {integration, [], integration()}
    , {evaluate,    [], evaluate()}
    , {parse,       [], parse()}
    , {scan,        [], scan()}
    ].

integration() ->

    [ integration
    , example
    ].

evaluate() ->

    [ simple
    , quote
    ].

parse() ->

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

symbol(_) ->
    [open, "foo", close] = fable:scan("(foo)").

integer(_) ->
    [open, 1024, close] = fable:scan("(1024)").

symbols(_) ->
    [open, "foo", "bar", "baz", close] = fable:scan("(foo bar baz)").

operator(_) ->
    [open, [$!], 1024, 512, close] = fable:scan("(! 1024 512)").

nest(_) ->
    [open,
     "foo",
      open, [$!], 1024, close,
      open, "baz", close,
     close] = fable:scan("(foo (! 1024) (baz))").
