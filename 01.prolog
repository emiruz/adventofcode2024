acc(_{0:_, l1:X, l2:Y}, Xs-Ys, [X|Xs]-[Y|Ys]).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []),
    re_foldl(acc, "(?<l1_I>\\d+) +(?<l2_I>\\d+)", S, []-[], Xs_-Ys_, []),
    maplist(msort, [Xs_,Ys_], [Xs,Ys]),
    aggregate_all(sum(abs(X-Y)), (nth0(Idx,Xs,X), nth0(Idx,Ys,Y)), Part1),
    aggregate_all(sum(A*C), (member(A,Xs), aggregate_all(count, member(A,Ys), C)), Part2).
