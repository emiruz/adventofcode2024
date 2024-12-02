safe(Xs) :-
    Xs \= [],
    \+ (append(_, [A,B|_], Xs), A>=B, append(_, [C,D|_], Xs), D>=C),
    \+ (append(_, [A,B|_], Xs), (abs(A-B) =:= 0; abs(A-B) > 3)).

mod_then_safe(Xs) :- member(X, Xs), select(X, Xs, Ys), safe(Ys), !.

acc(_{0:_,n:X}, Xs, [X|Xs]).

nums(S,Xs) :- re_foldl(acc, "(?<n_I>\\d+)", S, [], Xs, []).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []),
    split_string(S, "\n", "", Ss),
    maplist(nums, Ss, Xs),
    aggregate_all(count, (member(X, Xs), safe(X)), Part1),
    aggregate_all(count, (member(X, Xs), mod_then_safe(X)), Part2).
