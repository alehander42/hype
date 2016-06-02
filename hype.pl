
:- set_prolog_flag(verbose, silent).

:- initialization main.

:- use_module(hypeHelpers).


% helpers
get([(Name, A) | _], Name, A).
get([_ | E], Name, Result) :- get(Name, E, Result).

extract((Args, Block), Args, Block).

zip([], [], []).
zip([Name | E], [Slot | S], [(Name, Slot) | Combined]) :-
    zip(E, S, Combined).


eval([if, A, B, C], Env, XEnv, Result) :- 
    eval(A, Env, Env, R), R, eval(B, Env, XEnv, Result);
    eval(C, Env, XEnv, Result).

eval(Name, Env, Env, Result) :- atom(Name), get(Env, Name, Result).

eval([num, N], Env, Env, N).
eval([str, S], Env, Env, S).
eval([bool, B], Env, Env, B).

eval([math, +, L, R], Env, Env, Result) :-
    eval(L, Env, Env, L1),
    eval(R, Env, Env, R1),
    =(Result, L1 + R1).

eval([assign, T, V], Env, [(T, V1) | Env], V1) :- eval(V, Env, Env, V1).
%% eval([assign, T, V], [(T, V1) | Env], V1) :- eval(V, Env, V1).

eval([def, Name, Args, Block], Env, [(Name, Args, Block) | Env], Name).

eval([call, Name, Args], Env, Env, Res) :-
    get(Env, Name, F),
    extract(F, FArgs, FBlock),
    evalMap(Args, Env, Env, E),
    zip(FArgs, E, FEnv),
    list_combine(FEnv, Env, LambdaEnv),
    eval(FBlock, LambdaEnv, L2, Res).

eval([program | E], Env, XEnv, Result) :- evalAll(E, Env, XEnv, Result).

evalAll([Expr], Env, XEnv, Result) :- eval(Expr, Env, XEnv, Result).
evalAll([Expr | E], Env, XEnv, Result) :- eval(Expr, Env, XEnv0, _), evalAll(E, XEnv0, XEnv, Result).

evalMap([], Env, Env, []).
evalMap([Expr | E], Env, XEnv, [Result | Results]) :- eval(Expr, Env, XEnv0, Result), evalMap(E, XEnv0, XEnv, Results).

%% e(S, S + S).    

main :-
    %% eval([math, +, a, [num, 4]], [(a, 2)], E, Result),
    %% trace,
    eval([program, 
        [def, add2, [l2], 
            [math, +, l2, [num, 2]]],
        [call, add2, [[num, 46]]]
        ], [], E, Result),
    %% eval([program, [num, 2]], [], E, Result),
    X is Result,
    %% =(X, Result),
    %% get([(a, 2)], a, Result),
     writeln(X),
     writeln(E),

    halt.
main :-
    halt(1). 
