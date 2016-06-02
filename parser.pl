%% :- set_prolog_flag(verbose, silent).

%% :- initialization main.

:- module(lexer, [lex/2]).

:- use_module(hypeHelpers).

lex(Source, Tokens) :-
    name(Source, A),
    lex-list(A, [], ZZ),
    reverse(ZZ, Tokens).

% def
lex-list([100, 101, 102 | E], Input, Tokens) :-
    lex-list(E, [def | Input], Tokens).

% ' '
lex-list([32], Input, Input).

lex-list([32, S | E], Input, Tokens) :-
    non-ws(S),
    lex-list([S | E], [ws | Input], Tokens).

lex-list([32, 32 | E], Input, Tokens) :-
    lex-list([32 | E], Input, Tokens).

% '\n'
lex-list([10], Input, Input).

lex-list([10, S | E], Input, Tokens) :-
    non-nl(S),
    lex-list([S | E], [newl | Input], Tokens).

lex-list([10, 10 | E], Input, Tokens) :-
    lex-list([10 | E], Input, Tokens).

% '('
lex-list([40 | E], Input, Tokens) :-
    lex-list(E, [lparen | Input], Tokens).

% ')'
lex-list([41 | E], Input, Tokens) :-
    lex-list(E, [rparen | Input], Tokens).

lex-list([Let | E], Input, Tokens) :-
    is-let(Let),
    consume-name(E, Left, [Let], O),
    reverse(O, A),
    atom_codes(F, A),
    lex-list(Left, [[nam, F] | Input], Tokens).

lex-list([Num | E], Input, Tokens) :-
    is-num(Num),
    consume-number(E, Left, [Num], Z),
    reverse(Z, Vesela),
    number_codes(Nikola, Vesela),
    lex-list(Left, [[num, Nikola] | Input], Tokens).

lex-list([Op | E], Input, Tokens) :-
    is-op(Op),
    atom_codes(F, [Op]),
    lex-list(E, [[op, F] | Input], Tokens).


%% lex-list([Let, Let | E], Input)
lex-list([], Input, Input).

is-op(43). % +
is-op(45). % -

is-let(Let) :- char_type(Let, alpha).

is-num(Num) :- char_type(Num, digit).

is-alnum(Let) :- char_type(Let, alnum).

consume-number([], [], Q, Q).
consume-number([H | E], Left, Q, O) :-
    is-num(H),
    consume-number(E, Left, [H | Q], O).
consume-number([H | E], [H | E], Q, Q) :-
    \+ is-num(H).

consume-name([], [], Q, Q).
consume-name([H | E], Left, Q, O) :-
    is-alnum(H),
    consume-name(E, Left, [H | Q], O).
consume-name([H | E], [H | E], Q, Q) :-
    \+ is-alnum(H).



non-ws(A) :- A \= 32.

non-nl(A) :- A \= 10.

%% main :-
%%     %% eval([math, +, a, [num, 4]], [(a, 2)], E, Result),
%%     %% trace,
%%     %% eval([program, 
%%     %%     [def, add2, [l2], 
%%     %%         [math, +, l2, [num, 2]]],
%%     %%     [call, add2, [[num, 46]]]
%%     %%     ], [], E, Result),
%%     %% %% eval([program, [num, 2]], [], E, Result),
%%     %% X is Result,
%%     %% =(X, Result),
%%     %% get([(a, 2)], a, Result),
%%     %% trace,
%%     lex('def a(l)\n  l+2\nend\nl(2)', A),
%%      writeln(A),

%%     halt.
%% main :-
%%     halt(1). 
