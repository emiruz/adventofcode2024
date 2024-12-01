acc(_{0:_, l1:X, l2:Y}, L1-L2, [X|L1]-[Y|L2]).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []),
    re_foldl(acc, "(?<l1_I>\\d+) +(?<l2_I>\\d+)", S, []-[], L1_-L2_, []),
    maplist(msort, [L1_, L2_], [L1, L2]),
    aggregate_all(sum(abs(X-Y)), (nth0(Idx, L1, X), nth0(Idx, L2, Y)), Part1),
    aggregate_all(sum(A*C), (member(A, L1), aggregate_all(count, member(A, L2), C)), Part2).
