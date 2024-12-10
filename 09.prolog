add_idx(Xs, Ys) :- findall(I-X, nth1(I, Xs, X), Ys).

expand(Xs0, Ys) :-
    add_idx(Xs0, Xs),
    maplist([I0-X,L]>>(I is mod(I0,2)*(1+I0//2), length(L,X), maplist(=(I),L)), Xs, Xs2),
    flatten(Xs2, Xs3), maplist([A,B]>>(B is A-1), Xs3, Ys).

insert(Xs, Empty, Full, N, Ys) :-
    length(EmptyTrim, N), length(FullTrim, N),
    append(EmptyTrim, _, Empty), append(FullTrim, _, Full),
    maplist([I-A,J-B,C]>>(I<J->C=[J-A,I-B];C=[]), EmptyTrim, FullTrim, Swap0),
    flatten(Swap0, Swap), list_to_assoc(Swap, SwapDict),
    exclude({SwapDict}/[I-_]>>(get_assoc(I, SwapDict, _)), Xs, Rem),
    append(Rem, Swap, Final), sort(Final, Ys).

next_empty(_, N, Ys0, Ys) :- length(Ys0, N), reverse(Ys0, Ys), !.
next_empty([_-V|Xs], N, [], Ys) :- V \= -1, !, next_empty(Xs, N, [], Ys).
next_empty([X-(-1)|Xs], N, Ys, Out) :-  !, next_empty(Xs, N, [X-(-1)|Ys], Out).
next_empty([_-V|Xs], N, _, Ys) :- V \= -1, next_empty(Xs, N, [], Ys).

insert2([V|Vs], Xs, Final) :-
    findall(I-V, member(I-V, Xs), Full),
    length(Full, N),
    next_empty(Xs, N, [], Empty),
    insert(Xs, Empty, Full, N, NewXs),
    !, insert2(Vs, NewXs, Final).
insert2([_|Vs], Xs, Final) :- insert2(Vs, Xs, Final).
insert2([], Final, Final).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []), string_chars(S, Cs0),
    exclude(=('\n'), Cs0, Cs1), maplist(atom_number, Cs1, Cs),
    expand(Cs, Exp), add_idx(Exp, ExpIdx),
    include([I-A]>>(A=(-1)), ExpIdx, Empty),
    include([I-A]>>(A\=(-1)), ExpIdx, Full0), reverse(Full0, Full),
    length(Empty, N1), length(Full,N2), N is min(N1,N2),
    insert(ExpIdx, Empty, Full, N, Exp1),
    aggregate_all(sum(I*M), (nth0(I,Exp1,_-M), M \= -1), Part1),
    pairs_values(Full, Values0), sort(Values0,Values1), reverse(Values1,Values),
    insert2(Values, ExpIdx, Exp2),
    aggregate_all(sum(I*M), (nth0(I,Exp2,_-M), M \= -1), Part2).
