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

    [ {positive, [], success()}
    ].

success() ->

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

none(_) ->
    [] = fable:read("").

sentinal(_) ->
    [open, close] = fable:read("()").

spaced(_) ->
    [open, close] = fable:read("( )").

operator(_) ->
    [open, [$!], 1024, 512, close] = fable:read("(! 1024 512)").

integer(_) ->
    [open, 1024, close] = fable:read("(1024)").

symbol(_) ->
    [open, "foo", close] = fable:read("(foo)").

symbols(_) ->
    [open, "foo", "bar", "baz", close] = fable:read("(foo bar baz)").

nest(_) ->
    [open,
     "foo",
      open, [$!], 1024, close,
      open, "baz", close,
     close] = fable:read("(foo (! 1024) (baz))").
