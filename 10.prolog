:- use_module(library(clpfd)).

:- table value(_,_,sum).

coo(Idx0, Idx, X1-Y1) :-
    d(cols, Cols), d(rows, Rows),
    X0 #= mod(Idx0, Cols), Y0 #= Idx0 // Cols,
    X #= X0 + X1, Y #= Y0 + Y1,
    Rows #> X, Cols #> Y, X #>= 0, Y #>= 0,
    Idx #= X + Y * Cols.

value(Idx, Accept, 1) :- p(Idx, 9), memberchk(Idx, Accept), !.
value(Idx, Accept, Value) :-
    p(Idx, V0), member(Try, [0-1,0-(-1),1-0,-1-0]),
    coo(Idx, Idx1, Try), p(Idx1, V1),
    V1 #= V0 + 1,
    value(Idx1, Accept, Value).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []), string_chars(S,Cs0),
    nth0(Cols, Cs0, '\n'), !, exclude(=('\n'), Cs0, Cs),
    maplist(atom_number, Cs, CsNums), length(Cs, N), Rows is N//Cols,
    retractall(d(_,_)), asserta(d(rows, Rows)), asserta(d(cols, Cols)),
    findall(I-V, nth0(I, CsNums, V), CsIdx),
    retractall(p(_,_)), maplist([I-V]>>(asserta(p(I,V))), CsIdx),
    aggregate_all(count, (p(Idx1,0), p(Idx2,9), value(Idx1, [Idx2], _)), Part1),
    aggregate_all(sum(V), (p(Idx1,0), value(Idx1, _, V)), Part2).
