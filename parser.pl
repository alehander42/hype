:- module(parser, [parse/2]).

:- use_module(hypeHelpers).
:- use_module(lexer).

parse(Tokens, Ast) :-
    hprogram(Tokens, [], [], Ast).

maybe([Token | E], Token, Left) :- maybe(E, Token, Left).
maybe(Left, _, Left).

hprogram([], [], Input, [program | Z]) :- reverse(Input, Z).
hprogram([newl], [], Input, [program | Z]) :- reverse(Input, Z).
hprogram(Tokens, Left, Input, Ast) :-
    maybe(Tokens, newl, L),
    hstat(L, L1, [], S),
    =([S0], S),
    hprogram(L1, Left, [S0 | Input], Ast).

hstat(Tokens, Left, Input, Ast) :- hfunction(Tokens, Left, Input, Ast); hexpr(Tokens, Left, Input, Ast).


hfunction([def, ws, [nam, Name], lparen | E], Left, Input, Ast) :-
    hpar(E, Expr0, [], Args),
    maybe(Expr0, ws, Expr),
    hexpr(Expr, [newl, [nam, end] | Left], [], B),
    =([[def, Name, Args, B] | Input], Ast).

hcall([[nam, Name], lparen | E], Left, Input, Ast) :-
    hargs(E, Left, [], Args),
    =([[call, Name, Args] | Input], Ast).

hpar([rparen, newl | Left], Left, Ast, Ast).
hpar([[nam, Name], ws | L], Left, Input, Ast) :-
    hpar(L, Left, [Name | Input], Ast).
hpar([[nam, Name], rparen | L], Left, Input, Ast) :-
    reverse(I, [Name | Input]),
    hpar([rparen | L], Left, I, Ast).

hargs([rparen | Left], Left, Ast, Ast).
hargs([ws | E], Left, Input, Ast) :- hargs(E, Left, Input, Ast).
hargs(E, Left, Input, Ast) :-
    hexprnocall(E, G, [], Q),
    hargs(G, Left, [Q | Input], Ast).

hexpr(Tokens, Left, Input, Ast) :-
    hcall(Tokens, Left, Input, Ast);
    hmath(Tokens, Left, Input, Ast);
    hliteral(Tokens, Left, Input, Ast).

hexprnocall(Tokens, Left, Input, Ast) :-
    hmath(Tokens, Left, Input, Ast);
    hliteral(Tokens, Left, Input, Ast).
hliteral([[num, Num] | Left], Left, _, [num, Num]).
hliteral([[str, Str] | Left], Left, _, [str, Str]).
hliteral([[nam, Nam] | Left], Left, _, Nam).

hmath(E, Left, _, Ast) :-
    hliteral(E, [[op, Op] | L], [], H), hliteral(L, Left, [], R), =(Ast, [math, Op, H, R]).
