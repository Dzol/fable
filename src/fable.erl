%% ===================================================================
%% @copyright 2016 Joseph Yiasemides
%% @author <joseph.yiasemides@erlang-solutions.com>
%% @end
%% ===================================================================

-module(fable).
-export([parse/1]).
-export([scan/1]).
-import(lists, [reverse/1]).

-define(SPACE, $\ ).


%% -------------------------------------------------------------------
%% Interface
%% -------------------------------------------------------------------

parse(S) ->
    {[], T} = parse(S, []),
    T.

%% `++/2` seems more readable.
parse([], S) ->
    {[], S};
parse([open|M], S) ->
    {N, T} = parse(M, []),
    parse(N, S ++ [T]);
parse([close|M], S) ->
    {M, S};
parse([X|M], S) ->
    parse(M, S ++ [X]).

%% parse([], S) ->
%%     {[], reverse(S)};
%% parse([open|M], S) ->
%%     {N, T} = parse(M, []),
%%     parse(N, [T|S]);
%% parse([close|M], S) ->
%%     {M, reverse(S)};
%% parse([X|M], S) ->
%%     parse(M, [X|S]).

scan("") ->
    [];
scan([$(|Rest]) ->
    [open|scan(Rest)];
scan([$)|Rest]) ->
    [close|scan(Rest)];
scan([?SPACE|Rest]) ->
    scan(Rest);
scan([H|Rest])
  when $! == H; $% == H; $* == H; $+ == H; $- == H;
       $< == H; $= == H; $> == H; $^ == H; $~ == H ->
    [operator(Rest, H)|scan(Rest)];
scan([H|Rest]) when $0 =< H, H =< $9 ->
    {S, More} = integer(Rest, [H]),
    [S|scan(More)];
scan([H|Rest]) when $a =< H, H =< $z; $A =< H, H =< $Z ->
    {S, More} = symbol(Rest, [H]),
    [S|scan(More)].


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
