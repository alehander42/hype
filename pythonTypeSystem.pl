:- module(pythonTypeSystem, [list_/2,
    is_number/1, assign/3, call_/3, mcall_/4, fcall_/3, math/3,
    log/3, cmp/3, eq/3, first/2,
    if/4, for/4, m/4, sequence/1, f/3]).

list_([], _).
list_([H], [list, H]).
list_([H, H | T], [list, H]) :- list_(T, [list, H]). % hom list

dict_([], _).
dict_([(Y, Z)], [dict, Y, Z]) :- hashable(Y).
dict_([(Y, Z), (Y, Z) | T], [dict, Y, Z]) :- hashable(Y), dict_(T, [dict, Y, Z]). % hom dict

comparable(int).
comparable(float).

is_number(int).
is_number(float).

sequence([list, _]).
sequence([dict, _, _]).

hashable(int).
hashable(float).
hashable(string).
hashable(bool).


element_of(A, [list, A]).
element_of(A, [dict, A, _]).

assign(A, A, z).
call_(Function, Args, Result)   :- f(Function, Args, Result).
mcall_(Receiver, Message, Args, Result) :- m(Receiver, Message, Args, Result).
fcall_([function, Args, Res], Args, Res).

math(int, int, int).
math(A, B, float) :- is_number(A), is_number(B).
union([union | T], T).
log(_, _, bool).
cmp(A, A, bool)     :- comparable(A).
eq(A, A, bool).
first(A, X)         :- list(A, X).

if(bool, _, _, z). % test if_true if_false result
for(Element, Sequence, _, z) :- sequence(Sequence), element_of(Element, Sequence).


m([list, A], append, [A], z).
m([list, A], pop, [], A).
m([list, A], extend, [[list, A]], z).
m([list, A], index, [A], int).
m([list, A], index, [A, int], int).
m([list, A], index, [A, int, int], int).


m([dict, A, B], get, [A, B], B).
m([dict, A, _], keys, [list, A]).
m([dict, _, A], values, [list, A]).
m(string, join, [[list, string]], string).
m(string, split, [string], [list, string]).

f(len, [[list, _]], int).
f(as_int, [string], int).
f(display, [A], z)      :- string(A).
f(sorted, [[list, A]], [list, A]).

%% f([function, Args, Result], Args, Result).


% A, X
%% f(fac, [A], X) :-
%%     cmp(A, int, A1), % number == 1
    
%%     math(A, int, A2), % number - 2
%%     =([A2], [A]), % fac(number - 2)
%%     math(A, int, A3), % number - 1
%%     =([A3], [A]), % fac(number - 1) 
%%     math(X, X, A4), % fac(number - 1) + fac(number - 2)

%%     if(A1, int, A4, X). % if <cond> <true> <else>




