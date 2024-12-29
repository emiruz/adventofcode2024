solve(In, Part1) :-
    read_file_to_string(In, S0, []),
    re_replace("\n\n"/g, "x", S0, S1), re_replace("\n"/g, "", S1, S),
    re_foldl([_{0:X},[X|V0],V0]>>true, "[#//.]+",S,Ss,[],[]),
    maplist([X,Ys]>>string_chars(X,Ys), Ss, Opts),
    include([['#'|_]]>>true, Opts, Locks), include([['.'|_]]>>true, Opts, Keys),
    aggregate_all(count, (member(K, Keys), member(L, Locks),
			  maplist([X,Y,_]>>(\+ (X='#',Y='#')), K, L, _)), Part1).
