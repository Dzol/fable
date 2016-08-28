%% ===================================================================
%% @copyright 2016 Joseph Yiasemides
%% @author <joseph.yiasemides@erlang-solutions.com>
%% @end
%% ===================================================================

-module(fable).
-export([read/1]).
-import(lists, [reverse/1]).

-define(SPACE, $\ ).


%% -------------------------------------------------------------------
%% Interface
%% -------------------------------------------------------------------

read("") ->
    [];
read([$(|Rest]) ->
    [open|read(Rest)];
read([$)|Rest]) ->
    [close|read(Rest)];
read([?SPACE|Rest]) ->
    read(Rest);
read([H|Rest])
  when $! == H; $% == H; $* == H; $+ == H; $- == H;
       $< == H; $= == H; $> == H; $^ == H; $~ == H ->
    [operator(Rest, H)|read(Rest)];
read([H|Rest]) when $0 =< H, H =< $9 ->
    {S, More} = integer(Rest, [H]),
    [S|read(More)];
read([H|Rest]) when $a =< H, H =< $z; $A =< H, H =< $Z ->
    {S, More} = symbol(Rest, [H]),
    [S|read(More)].


%% -------------------------------------------------------------------
%% Ancillary
%% -------------------------------------------------------------------

operator([?SPACE|_], O) -> [O].

integer([Hd|Rest]=A, Lobmys) ->
    case number(Hd) of
        true ->
            integer(Rest, [Hd|Lobmys]);
        false ->
            {integerize(reverse(Lobmys)), A}
    end.

symbol([Hd|Rest]=A, Lobmys) ->
    case letter(Hd) of
        true ->
            symbol(Rest, [Hd|Lobmys]);
        false ->
            {reverse(Lobmys), A}
    end.

number(X) when $0 =< X, X =< $9 ->
    true;
number(_) ->
    false.

integerize(X) ->
    erlang:list_to_integer(X).

letter(X) when $A =< X, X =< $Z; $a =< X, X =< $z ->
    true;
letter(_) ->
    false.
