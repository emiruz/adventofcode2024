:- use_module([library(yall),library(rbtrees)]).

t_check(L) :- member(X,L), string_concat("t",_,X), !.

triangle(A,B,C) :- p(A,B), p(A,C), p(B,C), t_check([A,B,C]), msort([A,B,C],[A,B,C]).

combo(_, 0, []).
combo([H|T], Len0, [H|Subset]) :- Len0 > 0, Len is Len0-1, combo(T, Len, Subset).
combo([_|T], Len0, Subset) :- Len0 > 0, combo(T, Len0, Subset).

findit(Nodes, Len, Max) :-
    rb_empty(Empty),
    foldl({Len}/[N,V0,V]>>(
	      findall(X, p(N,X), Xs0), sort([N|Xs0], Xs1),
	      findall(X, combo(Xs1, Len, X), Xss),
	      foldl([Xs,V2,V3]>>(
		     (rb_lookup(Xs,C,V2), T is C+1, rb_insert(V2,Xs,T,V3),!;
		      rb_insert(V2, Xs, 1, V3))), Xss, V0, V
		   )), Nodes, Empty, Full),
    rb_visit(Full, Pairs0), include([X-C]>>length(X,C), Pairs0, Pairs),
    foldl([X-C,X0-C0,V]>>(C>C0->V=X-C;V=X0-C0), Pairs, _-0, Max-_).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []),
    re_foldl([_{0:_, n:N1, m:N2},[N1-N2|V0],V0]>>true, "(?<n>[a-z]+)-(?<m>[a-z]+)"/t, S, Ns, [], []),
    retractall(p(_,_)), maplist([N-M]>>(asserta(p(N,M)), asserta(p(M,N))), Ns),
    retractall(n(_)), findall(Node, p(Node,_), Nodes0), sort(Nodes0, Nodes),
    maplist([N]>>asserta(n(N)), Nodes),
    maplist([N,C]>>aggregate_all(count, p(N,_), C), Nodes, Degrees),
    max_list(Degrees, MaxDegree),
    aggregate_all(count, triangle(_,_,_), Part1),
    between(0, MaxDegree, Degree0), Degree is MaxDegree - Degree0,
    findit(Nodes, Degree, Part2), !.
