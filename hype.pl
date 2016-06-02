:- set_prolog_flag(verbose, silent).

:- initialization main.

:- use_module(lexer).
:- use_module(parser).
:- use_module(evaluator).

run(Source, Env, Result) :-
    lex(Source, Tokens),
    parse(Tokens, Ast),
    eval(Ast, Env, _, Result).

to-ast(Source, _, Ast) :-
    lex(Source, Tokens),
    parse(Tokens, Ast).

main :-
    run('def a(x)\n  x+48\nend\na(2)', [], Result),
    %% writeln(A),
    %% parser:parse(A, Ast),
    %% writeln(Ast),
    %% trace,
    %% eval(Ast, [], _, Result),
    %% lex
    %% eval([program, 
    %%     [def, add2, [l2], 
    %%         [math, +, l2, [num, 2]]],
    %%     [call, add2, [[num, 46]]]
    %%     ], [], E, Result),
    %% eval([program, [num, 2]], [], E, Result),
    X is Result,
    %% =(X, Result),
    %% get([(a, 2)], a, Result),
     writeln(X),
     %% writeln(E),

    halt.
main :-
    halt(1). 
