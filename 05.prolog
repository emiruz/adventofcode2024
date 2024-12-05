:- use_module(library(clpfd)).

list_to_nums(X,Y) :- split_string(X,",","",Y0), maplist(atom_number,Y0,Y).

partial_sort(Cs0, Xs, Ys) :-
    length(Ref,100), Ref ins 1..100,
    findall(A-B, (member(A-B, Cs0), (memberchk(A,Xs), memberchk(B,Xs))), Cs),
    maplist({Ref}/[I1-I2]>>(nth1(I1,Ref,A), nth1(I2,Ref,B), A #< B), Cs),
    label(Ref), !,

    maplist({Ref}/[I,X]>>nth1(I,Ref,X), Xs, Vs),
    pairs_keys_values(Pairs, Vs, Xs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Ys).

mid_point(X, Y) :- length(X, N0), N is 1 + N0 // 2, nth1(N, X, Y).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []),
    string_concat(Rest, Data, S), string_concat(Cons, "\n\n", Rest),
    re_foldl([_{0:_,a:A, b:B},V0,[A-B|V0]]>>true,"(?<a_I>\\d+)\\|(?<b_I>\\d+)",Cons,[],Cs,[]),

    split_string(Data, '\n', [], Data1), exclude(=(""), Data1, Data2),
    maplist(list_to_nums, Data2, Data3),

    aggregate_all(sum(N), (member(X,Data3), partial_sort(Cs, X, X), mid_point(X, N)), Part1),
    aggregate_all(sum(N), (member(X,Data3), partial_sort(Cs, X, Y), X \= Y, mid_point(Y,N)), Part2), !.
