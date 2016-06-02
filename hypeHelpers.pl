:- module(hypeHelpers, [list_combine/3]).

list_combine([], L, L).
list_combine([H | T], L, [H | X]) :- list_combine(T, L, X).

%% reverse_([]) --> [].
%% reverse_([H | T]) --> reverse_(T), [H].

%% reverse(L, M) :- phrase(reverse_, L, M).
