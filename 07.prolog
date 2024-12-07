test([X|Xs], Total0, Extra, Target) :-
    Target >= Total0,
    (  Total is Total0+X
    ;  Total is Total0*X
    ;  Extra = true, Total is X + Total0*10**(floor(log10(X))+1)),
    test(Xs, Total, Extra, Target), !.
test([], Target, _, Target).

check(S, Total1-Total2) :-
    re_foldl([_{0:_,n:N},V0,[N|V0]]>>true, "(?<n_I>\\d+)", S, [], Ns0, []),
    reverse(Ns0, [Total,Head|Rest]),
    (  test(Rest, Head, false, Total) -> Total1 = Total, Total2 = Total
    ;  test(Rest, Head, true, Total),    Total1 = 0,     Total2 = Total), !.
check(_,0-0).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []), split_string(S, "\n", "", Ss),
    maplist(check, Ss, Totals),
    aggregate_all(sum(A), member(A-B, Totals), Part1),
    aggregate_all(sum(max(A,B)), member(A-B, Totals), Part2).
