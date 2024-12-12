:- table expand/3.

expand_list(N0, [X|Xs0], Y) :-
    !, maplist(expand(N0), [X|Xs0], Xs1), sumlist(Xs1, Y).

expand(0, Xs, Y) :- is_list(Xs), length(Xs, Y), !.
expand(0, _, 1) :- !.
expand(N0, 0, Ys) :- N is N0-1, !, expand(N, 1, Ys).
expand(N0, X, Ys) :-
    N is N0-1,
    atom_string(X, Xstr), string_length(Xstr, M),
    mod(M, 2) =:= 0, Mnew is M // 2,
    string_concat(S1, S2, Xstr),
    string_length(S1, Mnew), string_length(S2, Mnew),
    atom_number(S1, Snum1), atom_number(S2, Snum2),
    !, expand_list(N, [Snum1, Snum2], Ys).
expand(N0, X0, Ys) :- N is N0-1, X is X0*2024, !, expand(N, X, Ys).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S0, []), re_replace("\n", "", S0, S),
    split_string(S, " ", "", Ss), maplist(atom_number, Ss, Ns),
    expand_list(25, Ns, Part1), expand_list(75, Ns, Part2).
