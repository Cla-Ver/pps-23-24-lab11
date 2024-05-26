% === PART 1 ===
search(E, List) :- member(E, List), !.

search2(E, [E, E | _]).
search2(E, [_ | T]) :- search2(E, T).

search_two(E, [E | T]) :- search(E, T).
search_two(E, [_ | T]) :- search_two(E, T).

size([], 0).
size([_ | T], N) :- size(T, N2), N is N2 + 1.

sum([], 0).
sum([E | T], N) :- number(E), sum(T, N2), N is N2 + E.

max([E], E, E).
max([H | T], H, Min) :- max(T, Max, Min), Max < H.
max([H | T], Max, H) :- max(T, Max, Min), Min > H.

split([], _, [], []).
split(List, 0, [], List).
split([H | T], N, Sublist1, Sublist2) :- N2 is N - 1, split(T, N2, L1, Sublist2), append([H], L1, Sublist1).

rotate([E | T], R) :- append(T, [E], R).

dice(X) :- member(X, [1, 2, 3, 4, 5, 6]).

% === PART 2 ===

dropAny(X, [X | T], T).
dropAny(X, [H | Xs], [H | L]) :- dropAny(X, Xs, L).

dropFirst(X, [Y | T], T) :- copy_term(X, Y), !.
dropFirst(X, [H | Xs], [H | L]) :- dropFirst(X, Xs, L).

dropLast(X, List, Res) :- reverse(List, RevList), dropFirst(X, RevList, Drop), reverse(Drop, Res).

dropAll(X, List, R) :- dropFirst(X, List, Dlist), dropAll(X, Dlist, R), !.
dropAll(X, List, List) :- member(X, List) \= true.


% === PART 3 ===
last([H1, H2 | T], R) :- !, last([H2 | T], R).
last([H | T], H).

fromList([_], []).
fromList([H1, H2 | T], [e(H1, H2) | L]) :- fromList([H2 | T], L). 

fromCircList([H | T], List) :- last([H | T], La), fromList([H | T], Li), append(Li, [e(La, H)], List).

outDegree([], _, 0).
outDegree([e(S, _) | T], S, N2) :- outDegree(T, S, N), N2 is N + 1.
outDegree([e(S2, _) | T], S, N) :- !, outDegree(T, S, N).

dropNode(G, N, OG) :- dropAll(e(N, _), G, G2), dropAll(e(_, N), G2, OG).

reaching([], _, []).
reaching([e(S, F) | T], S, L) :- reaching(T, S, List), !, append(List, [F], Li), reverse(Li, L).
reaching([e(S2, _) | T], S, L) :- reaching(T, S, L).

insertOrdered([], E, [E]).
insertOrdered([H | T], E, [E, H | T]) :- E < H, !.
insertOrdered([H | T], E, R) :- insertOrdered(T, E, L), append([H], L, R).

%Set implementation
listSet(List, E, List) :- member(E, List), !.
listSet(List, E, R) :- insertOrdered(List, E, R).

nodes([], []).
nodes([e(S, F) | T], List) :- nodes(T, L), listSet(L, S, L1), listSet(L1, F, List).

anypath([e(Start, Dest) | T], Start, Dest, [e(Start, Dest)]).
anypath([e(Start, Tr) | T], Start, Dest, R) :- anypath(T, Tr, Dest, Path), append(Path, [e(Start, Tr)], R).
anypath([Node | T], Start, Dest, R) :- anypath(T, Start, Dest, R).

allreaching(Graph, Node, List) :- nodes(Graph, AllNodes), allreaching(Graph, Node, AllNodes, List).
allreaching(_, _, [], []).
allreaching(Graph, Node, [H | T], List) :- anypath(Graph, Node, H, R), !, allreaching(Graph, Node, T, V), listSet(V, H, List).
allreaching(Graph, Node, [H | T], List) :- allreaching(Graph, Node, T, List).

% === PART 4 ===

interval(A, B, A).
interval(A, B, X) :- A2 is A+1, A2 < B, interval(A2, B, X).

neighbour(A, B, A, B2) :- B2 is B+1.
neighbour(A, B, A,  B2) :- B2 is B-1.
neighbour(A, B, A2, B) :- A2 is A+1.
neighbour(A, B, A2, B) :- A2 is A-1.

gridlink(N, M, link(e(X, Y), e(X2, Y2))) :-
	interval(0, N, X),
	interval(0, M, Y),
	neighbour(X, Y, X2, Y2),
	X2 >= 0, Y2 >= 0, X2 < N, Y2 < M.

gridnodes(N, M, e(X, Y)) :-
	interval(0, N, X),
	interval(0, M, Y).
	
grid(N, M, List) :- findall(L, (gridnodes(N, M, L)), List).

anypath([e(StartX, StartY), e(DestX, DestY) | T], e(StartX, StartY), e(DestX, DestY), Hops, [e(StartX, StartY), e(DestX, DestY)]) :- neighbour(StartX, StartY, DestX, DestY), Hops >= 1, !.
anypath([e(StartX, StartY), e(TransX, TransY) | T], e(StartX, StartY), DestNode, Hops, R) :- neighbour(StartX, StartY, TransX, TransY), Hops > 0, R2 is Hops - 1, !, anypath([e(TransX, TransY) | T], e(TransX, TransY), DestNode, R2, Path), append(Path, [e(StartX, StartY)], R).
anypath([Node, TrNode | T], Start, Dest, Hops, R) :- anypath([Node | T], Start, Dest, Hops, R).

playercells([], _, []).
playercells([cell(Node, Player) | T], Player, List) :- playercells(T, Player, L), !, insertCellOrdered(L, Node, List).
playercells([_ | T], Player, List) :- playercells(T, Player, List).

insertCellOrdered([], E, [E]).
insertCellOrdered([e(X1, Y1) | T], e(X2, Y2), [e(X2, Y2), e(X1, Y1) | T]) :- X2 < X1, !.
insertCellOrdered([e(X1, Y1) | T], e(X2, Y2), [e(X2, Y2), e(X1, Y1) | T]) :- X2 == X1, Y2 < Y1, !.
insertCellOrdered([H | T], E, R) :- insertCellOrdered(T, E, L), append([H], L, R).

next(Table, Player, win(Player), NewTable) :- 	gridnodes(3, 3, G),
						not(member(cell(G, _), Table)), 
						insertCellOrdered(Table, cell(G, Player), NewTable),
						playercells(NewTable, Player, C),
						interval(0, 3, S),
						interval(0, 3, E),
						(anypath(C, e(S, 0), e(E, 2), 2, R); anypath(C, e(0, S), e(2, E), 2, R)).
next(Table, Player, nothing, NewTable) :- 	gridnodes(3, 3, G),
						not(member(cell(G, _), Table)),
						insertCellOrdered(Table, cell(G, Player), NewTable).

%Example: next([cell(e(0, 0), x), cell(e(0, 1), x), cell(e(2, 0), o), cell(e(2, 1), o)], x, W, T)