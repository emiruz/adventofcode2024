:- use_module([library(clpfd), library(yall)]).

:- table coo/3, connect/2.

coo(Idx0, Idx, X1-Y1) :-
    d(cols, Cols), d(rows, Rows),
    X0 #= mod(Idx0, Cols), Y0 #= Idx0 // Cols,
    X #= X0 + X1, Y #= Y0 + Y1,
    Rows #> X, Cols #> Y, X #>= 0, Y #>= 0,
    Idx #= X + Y * Cols.

adj(Idx1, Idx2, Try) :- adj(Idx1, Idx2, Try, [0-1,0-(-1),1-0,-1-0]). 
adj(Idx1, Idx2, Try, Tries) :-
    p(Idx1, V0), member(Try, Tries),
    coo(Idx1, Idx2, Try), p(Idx2, V0).

connect(Idx1, Idx1).
connect(Idx1, Idx2) :- adj(Idx1, Idx2, _).
connect(Idx1, Idx2) :- connect(Idx1, X), connect(X, Idx2).

corners(Idx, Count) :-
    findall(O, adj(Idx,_,O,[1-0,-1-0]), Hs),
    findall(O, adj(Idx,_,O,[0-1,0-(-1)]), Vs),
    findall(O, adj(Idx,_,O,[1-1,-1-1,-1-(-1),1-(-1)]), Ds),
    append(Hs, Vs, All),
    length(Hs, Nhs), length(Vs, Nvs), length(Ds, Nds),
    aggregate_all(count, adj(Idx,_,_,[1-0,-1-0]), Nhs),
    aggregate_all(count, adj(Idx,_,_,[0-1,0-(-1)]), Nvs),
    ( Nvs+Nhs=:=0, Count is 4;
      Nvs+Nhs=:=1, Count is 2;
      Nvs+Nhs=:=2, Nvs*Nhs=:=0, Count is 0;
      Nvs*Nhs>0, All = [A-_,_-D], adj(Idx,_,_,[A-D]), Count is 1;
      Nvs*Nhs>0, All = [A-_,_-D], \+ adj(Idx,_,_,[A-D]), Count is 2;
      Nvs+Nhs=:=3, Vs=[_-B], aggregate_all(count,adj(Idx,_,_,[1-B,-1-B]),Bds), Count is 2-Bds;
      Nvs+Nhs=:=3, Hs=[B-_], aggregate_all(count,adj(Idx,_,_,[B-1,B-(-1)]),Bds), Count is 2-Bds;
      Nvs+Nhs=:=4, Count is 4-Nds).

group([Idx|Xs], Acc, Out) :-
    findall(I, connect(Idx,I), Ids),
    aggregate_all(count, (member(A,Ids), adj(A,B,_), A<B, memberchk(B,Ids)), Adj),
    aggregate_all(sum(C), (member(A,Ids), corners(A,C)), Cor),
    length(Ids, Area), Per is 4 * Area - 2 * Adj,
    subtract(Xs, Ids, Rest), !, group(Rest, [Area-Per-Cor|Acc], Out).
group([], Out, Out).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []), string_chars(S,Cs0),
    nth0(Cols, Cs0, '\n'), !, exclude(=('\n'), Cs0, Cs),
    length(Cs, N), Rows is N//Cols, findall(I, nth0(I, Cs, _), Ids),
    retractall(d(_,_)), asserta(d(rows, Rows)), asserta(d(cols, Cols)),
    retractall(p(_,_)), maplist([I,V,_]>>(asserta(p(I,V))), Ids, Cs, _),
    group(Ids, [], Gs),
    aggregate_all(sum(A*B)-sum(A*C), member(A-B-C, Gs), Part1-Part2).
