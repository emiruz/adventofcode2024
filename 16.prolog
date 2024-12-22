:- use_module([library(clpfd),library(yall),library(rbtrees),library(heaps)]).

coo(Idx0, Idx, X1-Y1) :-
    cols(Cols),
    X0 #= mod(Idx0, Cols), Y0 #= Idx0 // Cols,
    X #= X0 + X1, Y #= Y0 + Y1,
    Idx #= X + Y * Cols, !.

adj(X-D0, Y-D, V) :-
    (D=D0,V=1;
     D is mod(D0+1,4), V=1001;
     D is mod(D0+2,4), V=2001;
     D is mod(D0-1,4), V=1001),
    nth0(D,[1-0,0-1,-1-0,0-(-1)],IJ),
    coo(X,Y,IJ), \+ (p(X,'#') ; p(Y,'#')).

lookup0(Dict, Key, _, V) :- rb_lookup(Key, V, Dict), !.
lookup0(_, _, Default, Default).

find_nodes([X|Xs], Dict, Acc, Final) :-
    memberchk(X,Acc), !, find_nodes(Xs, Dict, Acc, Final).
find_nodes([X-Y|Xs0], Dict, Acc0, Final) :-
    lookup0(Dict, prev-X-Y, [], Other),
    append(Other, Xs0, Xs), !,
    find_nodes(Xs, Dict, [X-Y|Acc0], Final).
find_nodes([], _, Final, Final).

test(Start,Dir,Alt-N-D,Di0-H0,Di-H) :-
    lookup0(Di0, dist-N-D, 1000000, Vdist),
    (Vdist >= Alt
    -> rb_insert(Di0, dist-N-D, Alt, Di1),
       lookup0(Di0, prev-N-D, [], Vprev),
       rb_insert(Di1, prev-N-D, [Start-Dir|Vprev], Di),
       add_to_heap(H0, Alt, N-D, H)
    ;  Di=Di0, H=H0).

walk(Heap-Dict, End, Acc, Visited) :-
    min_of_heap(Heap, Acc, End-Dir),
    find_nodes([End-Dir], Dict, [], Pairs),
    pairs_keys(Pairs,Keys), sort(Keys,Sorted),
    length(Sorted, Visited), !.
walk(Heap0-Dict0, End, Cost, Visited) :-
    get_from_heap(Heap0, Udist, Start-Dir, Heap1),
    findall(A-N-D, (adj(Start-Dir, N-D, Dist), A is Udist + Dist), More),
    foldl({Start,Dir}/[Alt-N-D,Di0-H0,Di-H]>>(
	      lookup0(Di0, dist-N-D, 1000000, Vdist),
	      (Vdist >= Alt
	      -> rb_insert(Di0, dist-N-D, Alt, Di1),
		 lookup0(Di0, prev-N-D, [], Vprev),
		 rb_insert(Di1, prev-N-D, [Start-Dir|Vprev], Di),
		 add_to_heap(H0, Alt, N-D, H)
	      ;  Di=Di0, H=H0)
	  ), More, Dict0-Heap1, Dict-Heap),
    !, walk(Heap-Dict, End, Cost, Visited).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []), string_chars(S,Cs0),
    nth0(Cols, Cs0, '\n'), !, exclude(=('\n'), Cs0, Cs),
    findall(I-V, nth0(I, Cs, V), Ids),
    retractall(cols(_,_)), asserta(cols(Cols)),
    retractall(p(_,_)), forall(member(I-V, Ids), asserta(p(I,V))),
    p(Start,'S'), p(End,'E'),
    singleton_heap(Heap, 0, Start-0), rb_empty(Dict),
    walk(Heap-Dict, End, Part1, Part2).
