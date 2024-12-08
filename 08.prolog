:- use_module(library(clpfd)).

coo(Cols, Idx, X-Y) :- X #= Idx // Cols, Y #= mod(Idx,Cols).

solve(In, P1, P2) :-
    read_file_to_string(In, S, []), string_chars(S,Cs0),
    nth0(Cols, Cs0, '\n'), !, exclude(=('\n'), Cs0, Cs),
    length(Cs, N), M is N-1,
    findall(
	Out,
	( nth0(I1, Cs, C1), C1 \= '.', nth0(I2, Cs, C1), I1 < I2,
	  coo(Cols, I1, X1-Y1), coo(Cols, I2, X2-Y2),
	  Idx in 0..M, coo(Cols, Idx, X0-Y0),
	  (X1-X0)*(Y2-Y0) #= (X2-X0)*(Y1-Y0),
	  D1 #= abs(X0-X1) + abs(Y0-Y1),
	  D2 #= abs(X0-X2) + abs(Y0-Y2),
	  label([Idx]),
	  (min(D1,D2) #= abs(D1-D2), Out=[Idx]-[Idx]; Out=[Idx]-[])
	), Ids),
    pairs_keys(Ids, Ks), pairs_values(Ids, Vs),
    maplist([L0,Len]>>(flatten(L0,L),sort(L,Srt),length(Srt,Len)), [Vs,Ks], [P1,P2]).
