:- use_module(library(clpfd)).

safety_factor(Ns, Sec, Out) :-
    maplist({Sec}/[[Px,Py,Vx,Vy],X-Y]>>(X is mod(Px+Sec*Vx,101), Y is mod(Py+Sec*Vy,103)), Ns, Out).

safety_factor_sum(Ns, Sec, Out) :-
    safety_factor(Ns, Sec, Final),
    msort(Final, Sorted),  clumped(Sorted, Sums),
    foldl([X-Y-V, [A0,B0,C0,D0], [A,B,C,D]]>>(
				     ((X=:=50;Y=:=51), A=A0,B=B0,C=C0,D=D0;
				       50>X,  51>Y, A is A0+V,B=B0,C=C0,D=D0;
				       X>50,  51>Y, A=A0,B is B0+V,C=C0,D=D0;
				       50>X,  Y>51, A=A0,B=B0,C is C0+V,D=D0;
				       X>50,  Y>51, A=A0,B=B0,C=C0,D is D0+V)),
	  Sums, [0,0,0,0], [A,B,C,D]), !, Out is A*B*C*D.

dist(Xs, Out) :-
    findall(X, member(X-_,Xs), Cols), msort(Cols, SortedCols),
    clumped(SortedCols, GroupsCols), pairs_values(GroupsCols, ColValues),
    foldl([X,V0,V]>>(V is V0 + X**2), ColValues, 1, Out1),

    findall(X, member(_-X,Xs), Rows), msort(Rows, SortedRows),
    clumped(SortedRows, GroupsRows), pairs_values(GroupsRows, RowValues),
    foldl([X,V0,V]>>(V is V0 + X**2), RowValues, 1, Out2),

    Out is Out1*Out2.

solve(In, Part1, Part2) :-
    read_file_to_string(In, Str, []), split_string(Str, "\n", "", Ls),
    Nums=[S,Ns]>>(re_foldl([_{0:N},[N|V0],V0]>>true, "[-0-9]+"/t, S,Ns,[],[]), Ns\=[]),
    convlist(Nums, Ls, Ns1), safety_factor_sum(Ns1, 100, Part1),

    T in 0..100000, T #> 1,
    maplist({T}/[[Px,Py,Vx,Vy]]>>(Px#=mod(Px+T*Vx,101), Py#=mod(Py+T*Vy,103)), Ns1),
    labeling([min(T)], [T]),

    !, findall(I, between(1,T,I), Ids),
    maplist({Ns1}/[Sec,V]>>(safety_factor(Ns1, Sec, New), dist(New, V0), V = V0-Sec ), Ids, Dists),
    sort(Dists, SortedDists), last(SortedDists, _-Part2).
