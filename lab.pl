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
dropAll(X, [H | Xs], [H | L]) :- dropAll(X, Xs, L).

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
% non-tail
inDegree([], N, 0).
inDegree([e(_, N) | T], N, D) :- inDegree(T, N, X), !, D is X + 1.
inDegree([H | T], N, X) :- inDegree(T, N, X).

% tail
% X is the node, N is the degree
inDegreeT(L, X, N) :-  inDegreeT(L, X, N, 0).
inDegreeT([], X, N, N).
inDegreeT([e(_, X) | T], X, N, Nold) :- N2 is Nold + 1, inDegreeT(T, X, N, N2), !. % tail
inDegreeT([e(_, Y) | T], X, N, Nold) :- inDegreeT(T, X, N, Nold).

% 2.4
% dropNode(+Graph, +Node, -OutGraph)
% drop all edges starting and leaving from a Node
safeDropAll(X, [], []).
safeDropAll(X, [Y | T], R) :- copy_term(X, Y), safeDropAll(X, T, R), !.
safeDropAll(X, [H | Xs], [H | L]) :- safeDropAll(X, Xs, L).

dropNode(G, N, OG) :- safeDropAll(e(N, _), G, G2), safeDropAll(e(_, N), G2, OG).

% 2.5
% reaching(+Graph, +Node, -List)
% all the nodes that can be reached in 1 step from Node
% possibly use findall, looking for e(Node, _) combined
% with member(?Elem, ?List)
reaching(G, N, L) :- findall(Y, member(e(N, Y), G), L).

% 2.6
% anypath(+Graph, +Node1, +Node2, -ListPath)
% a path from Node1 to Node2
% if there are many path , they are showed 1-by-1
% -- First solution: work poorly (some bugs) --
% anypathWrong([e(1,2),e(1,3),e(2,3)],1,3,L) -> work
% anypathWrong([e(0, 0), e(0,1), e(0, 2), e(1,0), e(1,1), e(1,2), e(2,0), e(2,1), e(2,2)], 1, 2, L) -> don't work
% spurious no problem!
anypathWrong([e(N1, N2) | _], N1, N2, [e(N1, N2)]).
anypathWrong([e(N1, N3) | T], N1, N2, [e(N1, N3) | L]) :- anypathWrong(T, N3, N2, L).
anypathWrong([H | T], N1, N2, L) :- anypathWrong(T, N1, N2, L).

% -- Second solution: find all paths, limited to 3 hops --
% anypath([e(1,2),e(1,3),e(2,3)],1,3,L) -> work
% anypath([e(0, 0), e(0,1), e(0, 2), e(1,0), e(1,1), e(1,2), e(2,0), e(2,1), e(2,2)], 1, 2, L) -> work
anypath(L, N1, N2, R) :- anypath(L, N1, N2, R, 3).
anypath(L, N1, N2, [e(N1, N3) | R], K) :- K > 1, K2 is K - 1, member(e(N1, N3), L), anypath(L, N3, N2, R, K2).
anypath(L, N1, N2, [e(N1, N2)], K) :- member(e(N1, N2), L), !.

% 2.7
% allreaching(+Graph, +Node, -List)
% all the nodes that can be reached from Node
% Suppose the graph is NOT circular!
% Use findall and anyPath!
allreaching(G, N, L) :- findall(Y, anypath(G, N, Y, L2), L).

% 2.8
% During last lesson we see how to generate a grid-like network. Adapt
% that code to create a graph for the predicates implemented so far.
% Try to generate all paths from a node to another, limiting the
% grid(+N, +K, -P)
% N -> number of nodes
% K -> maximum number of hops
% P -> Path
couple(N, X, Y) :- between(1, N, X), between(1, N, Y).

grid(N, K, P) :- 
	findall(e(X, Y), couple(N, X, Y), G), % Generating the whole graph 
	between(1, N, X1), % Iterate on all the couples
	between(1, N, Y1),
	anypath(G, X1, Y1, P, K). % Find the path from X1 to Y1
	