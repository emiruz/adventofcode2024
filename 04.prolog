o2c(Xs, N, A-B, Os0, Cs) :-
    include({N,A,B}/[X-Y]>>(A+X>=0,A+X<N,Y+B>=0,Y+B<N), Os0, Os),
    maplist({Xs,N,A,B}/[X-Y,C]>>(I is (A+X)+(Y+B)*N, nth0(I,Xs,C)), Os, Cs).

xmas(Cs, N, X) :-
    member(Os, [[1-0,2-0,3-0], [-1-0,-2-0,-3-0], [1-(-1),2-(-2),3-(-3)], [0-(-1),0-(-2),0-(-3)],
		[1-1,2-2,3-3], [-1-(-1),-2-(-2),-3-(-3)], [-1-1,-2-2,-3-3], [0-1,0-2,0-3]]),
    o2c(Cs, N, X, Os, ['M','A','S']).

x_mas(Cs, N, X) :-
    o2c(Cs, N, X, [-1-(-1),1-(-1),-1-1,1-1], Out),
    memberchk(Out, [['M','S','M','S'], ['S','M','S','M'], ['M','M','S','S'], ['S','S','M','M']]).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []), string_chars(S,Cs0),
    nth0(N, Cs0, '\n'), !, exclude(=('\n'), Cs0, Cs),
    aggregate_all(count, (nth0(I,Cs,'X'), X is mod(I,N), Y is I//N, xmas(Cs,N,X-Y)), Part1),
    aggregate_all(count, (nth0(I,Cs,'A'), X is mod(I,N), Y is I//N, x_mas(Cs,N,X-Y)), Part2).
