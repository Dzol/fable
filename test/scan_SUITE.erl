%% ===================================================================
%% @copyright 2016 Joseph Yiasemides
%% @author <joseph.yiasemides@erlang-solutions.com>
%% @end
%% ===================================================================

-module(scan_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%% -------------------------------------------------------------------
%% Suite description
%% -------------------------------------------------------------------

all() ->

    [ {group, scan}
    ].

groups() ->

    [ {scan, [], scan()}
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
