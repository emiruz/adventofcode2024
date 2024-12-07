:- use_module(library(clpfd)).

check(S, Total) :-
    re_foldl([_{0:_,n:N},V0,[N|V0]]>>true, "(?<n_I>\\d+)", S, [], Ns0, []),
    reverse(Ns0, [Total,Head|Rest]),
    length(Rest, N), length(Cons, N), Cons ins 0..1,
    foldl([X,C,V0,V]>>(V#>=V0, V#=C*(V0+X)+(1-C)*(V0*X)), Rest, Cons, Head, Total),
    label(Cons), !.
check(_, 0).

solve(In, Part1) :-
    read_file_to_string(In, S, []), split_string(S, "\n", "", Ss),
    maplist(check, Ss, Totals), sumlist(Totals, Part1).
