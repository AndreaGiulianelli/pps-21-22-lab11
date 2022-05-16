% dropAny(?Elem, ?List, ?OutList)
dropAny(X, [X | T], T).
dropAny(X, [H | Xs], [H | L]) :- dropAny(X, Xs, L).

% ======= 1 =======
% 1.1
% dropFirst: drops only the first occurrence (showing no alternative results)
dropFirst(X, L, T) :- dropAny(X, L, T), !.

% dropLast: drops only the last occurrence (showing no alternative results)
dropLast(X, [H | Xs], [H | L]) :- dropLast(X, Xs, L), !.
dropLast(X, [X | T], T).

% dropAll: drop all occurrences, returning a single list as result
dropAll(X, [], []).
dropAll(X, [X | T], R) :- dropAll(X, T, R), !.
dropAll(X, [H | Xs], [H | L]) :- dropAll(X, Xs, L), !.

% ======= 2 =======
% 2.1
% fromList (+ List , - Graph )
fromList([_], []).
fromList([H1, H2 | T], [e(H1, H2) | L]) :- fromList([H2 | T], L).

% 2.2
% fromCircList(+ List, -Graph)
fromCircList([H | T], G) :- fromCircList([H | T], G, H).
fromCircList([X], [e(X, H)], H). 
fromCircList([H1, H2 | T], [e(H1, H2) | L], H) :- fromCircList([H2 | T], L, H).

% 2.3
% inDegree(+Graph, +Node, -Deg)
% Deg is the number of edges leading into Node
% TODO: non-tail

% tail
inDegree(L, X, N) :-  inDegree(L, X, N, 0).
inDegree([], X, N, N).
inDegree([e(_, X) | T], X, N, Nold) :- N2 is Nold + 1, inDegree(T, X, N, N2), !. % tail
inDegree([e(_, Y) | T], X, N, Nold) :- inDegree(T, X, N, Nold).


