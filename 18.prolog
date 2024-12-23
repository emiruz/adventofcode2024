:- use_module([library(clpfd),library(yall),library(rbtrees),library(heaps)]).

adj(X-Y,I-J) :-
    d(cols,Cols),
    A #= X-I, B #= Y-J, member(A-B, [0-1,1-0,-1-0,0-(-1)]),
    maplist(#=<(0), [X,Y,I,J]), maplist(#>(Cols), [X,Y,I,J]),
    \+ p(X-Y), \+ p(I-J).

lookup0(Dict, Key, _, V) :- rb_lookup(Key, V, Dict), !.
lookup0(_, _, Default, Default).

walk_(Heap-_, End, Acc) :- min_of_heap(Heap, Acc, End),!.
walk_(Heap0-Dict0, End, Cost) :-
    get_from_heap(Heap0, Udist, Start, Heap1),
    findall(A-N, (adj(Start,N), A is Udist+1), More),
    foldl({Start}/[Alt-N,Di0-H0,Di-H]>>(
	      lookup0(Di0, dist-N, 1000000, Vdist),
	      (Vdist > Alt
	      -> rb_insert(Di0, dist-N, Alt, Di),
		 add_to_heap(H0, Alt, N, H)
	      ;  Di=Di0, H=H0)
	  ), More, Dict0-Heap1, Dict-Heap),
    !, walk_(Heap-Dict, End, Cost).
walk(Start, End, Cost) :-
    singleton_heap(Heap, 0, Start), rb_empty(Dict),
    walk_(Heap-Dict, End, Cost).

simulate([[X,Y]|_],X-Y) :- retract(p(X-Y)), walk(0-0,70-70,_), !.
simulate([_|Xs],X) :- !, simulate(Xs,X).
simulate([],_) :- false.

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []), split_string(S, "\n", "", Ss),
    convlist([X,Y]>>(split_string(X,",","",X1), maplist(atom_number,X1,Y)), Ss, Ns),
    retractall(d(_,_)), maplist(asserta, [d(cols,71), d(start,0-0), d(end,70-70)]),
    length(Prefix, 1024), append(Prefix, _, Ns),
    retractall(p(_)), maplist([[X,Y]]>>asserta(p(X-Y)),Prefix), walk(0-0,70-70,Part1),
    retractall(p(_)), maplist([[X,Y]]>>asserta(p(X-Y)),Ns),
    reverse(Ns, NsRev), simulate(NsRev, Part2).
