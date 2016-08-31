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

%% @doc The `scan/1` procedure takes a string of characters and
%% produces a list of tokens which are more amenable to processing,
%% and thus parsing, by machine. The symbols `open` and `close`
%% represent opening and closing parentheses respectively. They can be
%% thought of as constants or enumerated constants in some sense. In
%% fact that is exactly what they are underneath. They're here just
%% for the intrinsic meaning we give them. Integer literals produce
%% integer values right away and LISP atoms or symbols produce Erlang
%% strings right away. We don't do string literals and we don't
%% understand LISP quoting (if we really want it then we can build an
%% interpreter or compiler with a procedure called `quote`). All these
%% clauses push to the stack and press on with a recursive call to
%% scan the remaining characters in the string.
scan("") ->
    %% When we see an empty string then we know there are no tokens in
    %% it so we return the empty list.
    [];
scan([$(|Rest]) ->
    %% An opening parenthesis, `$(`, produces the symbol `open`.
    [open|scan(Rest)];
scan([$)|Rest]) ->
    %% An closing parenthesis, `$)`, produces the symbol `close`.
    [close|scan(Rest)];
scan([?SPACE|Rest]) ->
    %% Spaces are ignored: we should also ignore other white-space!
    scan(Rest);
scan([H|Rest])
  when $! == H; $% == H; $* == H; $+ == H; $- == H;
       $< == H; $= == H; $> == H; $^ == H; $~ == H ->
    %% When we see one of a selection of single character operators
    %% then we call the `operator` procedure to advance us one
    %% character which must be a SPACE character (see the definition
    %% below). In other words we don't allow LISP symbols or atoms to
    %% start with one of the operators we recognise though they could
    %% start with one we don't recognise. This clause is here because
    %% with the clause for digits and letters below we would not get
    %% the operator chararacters.
    [operator(Rest, H)|scan(Rest)];
scan([H|Rest]) when $0 =< H, H =< $9 ->
    %% When we see a digit we collect the like characters of an
    %% integer by calling the `integer` procedure and then advance the
    %% scanner past the characters that make up the integer. This
    %% really saves us complexity if we'd combined the scanner and
    %% parser (but maybe that's what recursive-decent parses make
    %% tidy).
    {S, More} = integer(Rest, [H]),
    [S|scan(More)];
scan([H|Rest]) when $a =< H, H =< $z; $A =< H, H =< $Z ->
    %% When we see a letter we collect the like characters of a LISP
    %% symbol or atom by calling `symbol` and then advance the
    %% scanner past the characters that make up the symbol.
    {S, More} = symbol(Rest, [H]),
    [S|scan(More)].

%% @doc The `parse/1` procedure takes an Erlang list of tokens like
%% the list produced by `scan/1` and returns a parse tree. The parse
%% tree is an Erlang list too, but unlike the one produced by `scan/1`
%% (the lexical scanner), this list has nested lists to build a kind
%% of tree structure. The test cases called `tree` and `forest` are
%% really quite instructive. This discusses our represtentation a
%% little.  To illustrate: variable `Tokens` from the procedure's head
%% looks something like `[open, "foo", open, "bar", close, close]`
%% whose structure is completeley flat, while `Tree` from the
%% procedure's body looks something like `["foo", ["bar"]]` with one
%% list inside the other. So one way to look at the `parse` procedure
%% is to say that it builds a tree structure from a flat structure. A
%% list structure is a good representation for a LISP list because
%% LISP lists have arbitrary size so we can build a representation for
%% them by gradually building an Erlang list.
parse(Tokens) ->
    %% The tuple returned gives us a list of remaining tokens, `[]`,
    %% and the tree, `Tree`. The symbol for the empty list ,`[]`,
    %% along with the match operator, `=`, are a kind of assertion
    %% that there are no more remaining tokens. We pass the `parse/2`
    %% procedure a flat list of tokens, `Tokens`, and an empty list
    %% which is the representation for our initial tree which is
    %% simple a list of lists.
    {[], Tree} = parse(Tokens, []),
    %% At the end all we're really interested in is the tree. We
    %% return it here.
    Tree.

%% @doc The first parameter is the list of remaining tokens and the
%% second parameter is the tree that has been built so far. The tree
%% is built top-down left-right.
parse([], Tree) ->
    %% The empty list in the procedure's head above acts as a kind of
    %% assertion that we have no more tokens to parse. We return a
    %% tuple to keep all the return values of the `parse/2` procedure
    %% the same even though all we'd care about at this point is the
    %% tree, `Tree`.
    {[], Tree};
parse([open|Tokens], Parent) ->
    %% The pattern above indicates that we are entering a LISP list in
    %% our flat list of tokens. Below we parse the remaining tokens
    %% but start with a new tree, the `[]` as the second argument to
    %% our recursive call to `parse/2`, as we want to build a child
    %% tree, `Child`.
    {Remaining, Child} = parse(Tokens, []),
    %% Once we've got a complete child tree into `Child`, which may
    %% itself have its own children, we insert the child tree `Child`
    %% into the parent tree `Parent` by appending them (as they are
    %% lists in our represtentation). The call to `parse/2` will
    %% proceed with the remaining tokens and the most recent tree.
    parse(Remaining, Parent ++ [Child]);
parse([close|Tokens], Parent) ->
    %% When we see the `close` symbol we return the remaining tokens
    %% in `Tokens` and the tree we've built so far which might be a
    %% tree of depth one (in other words "a flat tree/list") but might
    %% not be.
    {Tokens, Parent};
parse([Sibling|Tokens], Tree) ->
    %% When we see any other token, `Sibling`, we just append it (or
    %% insert it) into the tree `Tree`. These symbols don't give us
    %% any information about structure we can introduce.
    parse(Tokens, Tree ++ [Sibling]).


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
