:- use_module(library(clpfd)).

optimise(X0-Y0, X1-Y1, X2-Y2, Offset, Tokens) :-    
    A #>= 0, B #>=0,
    X0+Offset #= A*X1 + B*X2, Y0+Offset #= A*Y1 + B*Y2,
    labeling([min(3*A+B)], [A,B]), Tokens #= 3*A+B.

take([Y0,X0,Y2,X2,Y1,X1|Xs], Offset, Acc0, Total) :-
    optimise(X0-Y0, X1-Y1, X2-Y2, Offset, Tokens), Acc is Acc0 + Tokens,
    !, take(Xs, Offset, Acc, Total).
take([_,_,_,_,_,_|Xs], Offset, Acc, Total) :- take(Xs, Offset, Acc, Total).
take([], _, Acc, Acc).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []),
    re_foldl([_{0:N},V0,[N|V0]]>>true, "\\d+"/t, S, [], Ns, []),
    take(Ns, 0, 0, Part1), take(Ns, 10000000000000, 0, Part2).
