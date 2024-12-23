:- table walk/2.

walk(S,N) :-
    opts(Os),
    aggregate_all(sum(N0),(member(O,Os), string_concat(O,T,S),
			   (T="" -> N0=1 ; walk(T,N0))), N).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []),
    string_concat(S0, S2, S), string_concat(S1, "\n\n", S0),
    re_foldl([_{0:X},[X|V0],V0]>>true, "[a-z]+", S1, Opts, [], []),
    re_foldl([_{0:X},[X|V0],V0]>>true, "[a-z]+", S2, Pats, [], []),
    retractall(opts(_)), asserta(opts(Opts)),
    convlist([X,N]>>(walk(X,N),N\=0), Pats, Counts),
    aggregate_all(count-sum(C), member(C,Counts), Part1-Part2).
