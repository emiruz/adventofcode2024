path(X-Y, Prev, Acc, Final) :-
    member(I-J, [0-1,1-0,-1-0,0-(-1)]), A is I+X, B is J+Y,
    \+ p(A,B,'#'), A-B \= Prev,
    !, path(A-B, X-Y, [X-Y|Acc], Final).
path(End, _, Acc, [End|Acc]).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []), string_chars(S,Cs0),
    nth0(Cols, Cs0, '\n'), !, exclude(=('\n'), Cs0, Cs),
    findall(I-J-C, (nth0(X,Cs,C), I is mod(X,Cols), J is X//Cols), Coo),
    retractall(p(_,_,_)), maplist([I-J-C]>>asserta(p(I,J,C)), Coo),
    p(X0,Y0,'S'), path(X0-Y0, none, [], Path),
    lazy_findall(P1-P2,
		 ( nth0(I1, Path, X-Y), nth0(I2, Path, I-J), I1<I2,
		   Man is abs(X-I) + abs(Y-J),
		   (2>=Man,  abs(I1-I2) - Man >= 100->P1=1; P1=0),
		   (20>=Man, abs(I1-I2) - Man >= 100->P2=1; P2=0) ), Calc),
    foldl([A-B,T1-T2,T3-T4]>>(T3 is T1+A, T4 is T2+B), Calc, 0-0, Part1-Part2).
