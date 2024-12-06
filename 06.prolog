:- table coo/5, ref/3.

coo(Cols, Rows, Idx0, X-Y, Idx) :-
    X0 is X + mod(Idx0, Cols), Y0 is Y + Idx0 // Cols,
    X0 >= 0, Rows > X0, Y0 >= 0, Cols > Y0,
    Idx is X0 + Y0 * Cols, Cols * Rows > Idx.

ref(Dir, Off, Turn) :-
    member(Dir-Turn-Off, ['<'-'^'-(-1-0),'>'-'v'-(1-0),'^'-'>'-(0-(-1)),'v'-'<'-(0-1)]).

walk(Pos-Dir, Cols, Rows, Os, Ps, Final) :-
    ref(Dir, Off, Next),
    (   coo(Cols, Rows, Pos, Off, NewPos),
        get_assoc(NewPos, Os, 0), !
    ->  walk(Pos-Next, Cols, Rows, Os, Ps, Final)
    ;   coo(Cols, Rows, Pos, Off, NewPos), !,
        \+ get_assoc(NewPos-Dir, Ps, 0),
        put_assoc(NewPos-Dir, Ps, 0, Ps1),
        walk(NewPos-Dir, Cols, Rows, Os, Ps1, Final)
    ;   assoc_to_keys(Ps, Vs0), pairs_keys(Vs0, Vs), sort(Vs, Final)).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []), string_chars(S, Cs0),
    nth0(N, Cs0, '\n'), exclude(=('\n'), Cs0, Cs),
    nth0(Start, Cs, C), memberchk(C, ['<','>','^','v']), !,
    length(Cs, Last), M is Last // N,
    findall(I-0, (nth0(I,Cs,O), O='#'), Os), list_to_assoc(Os, OsAssoc),
    list_to_assoc([(Start-C)-0], AssocP),
    walk(Start-C, N, M, OsAssoc, AssocP, U1), length(U1, Part1),
    aggregate_all(
	count,
	(member(P, U1), P\=Start, put_assoc(P, OsAssoc, 0, OsAssoc2),
	 \+ walk(Start-C, N, M, OsAssoc2, AssocP, _)), Part2).
