o2c(Xs, N, A-B, Os0, Cs) :-
    include({N,A,B}/[X-Y]>>(A+X>=0,A+X<N,Y+B>=0,Y+B<N), Os0, Os),
    maplist({N,A,B}/[X-Y,I]>>(I is (A+X) + (Y+B)*N), Os, Is),
    maplist({Xs}/[I,X]>>(nth0(I,Xs,X)), Is, Cs).

xmas(Cs, N, X) :-
    member(Os, [[1-0,2-0,3-0], [-1-0,-2-0,(-3)-0], [0-1,0-2,0-3],
		[0-(-1),0-(-2),0-(-3)], [1-1,2-2,3-3], [-1-(-1),-2-(-2),-3-(-3)],
                [1-(-1),2-(-2),3-(-3)], [-1-1,-2-2,-3-3]]),
    o2c(Cs, N, X, Os, ['M','A','S']).

x_mas(Cs, N, X) :-
    o2c(Cs, N, X, [(-1)-(-1),1-(-1),(-1)-1,1-1], Out),
    member(Out, [['M','S','M','S'], ['S','M','S','M'],
		 ['M','M','S','S'], ['S','S','M','M']]).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []), string_chars(S,Cs0),
    nth0(N, Cs0, '\n'), !, exclude(=('\n'), Cs0, Cs),
    findall(X-Y, (nth0(I, Cs, 'X'), X is mod(I, N), Y is I//N), Xs),
    aggregate_all(count, ( member(X, Xs), xmas(Cs, N, X) ), Part1),
    findall(X-Y, (nth0(I, Cs, 'A'), X is mod(I, N), Y is I//N), Xs2),
    aggregate_all(count, ( member(X, Xs2), x_mas(Cs, N, X) ), Part2).
