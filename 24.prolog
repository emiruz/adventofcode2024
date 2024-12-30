:- use_module([library(clpfd),library(yall),library(rbtrees)]).

ops("AND",A,B,C) :- C #= A*B.
ops("OR",A,B,C)  :- C #= A+B-A*B.
ops("XOR",A,B,C) :- C #= A xor B.

to_int([], _, Final, Final).
to_int([X|Xs], N0, Acc0, Final) :- !, Acc#=Acc0+X*(2^N0), N is N0+1, to_int(Xs,N,Acc,Final).
to_int(Xs0, Final) :- to_int(Xs0, 0, 0, Final).

prefix(D,Pre,Ks,Vs) :-
    rb_visit(D,Ps), include({Pre}/[K-_]>>string_concat(Pre,_,K), Ps, Valid),
    pairs_keys_values(Valid, Ks, Vs).
insert(D0,Ks,Xs,D) :- foldl([K,X,V0,V]>>rb_insert(V0,K,X,V), Ks, Xs, D0, D).

mix(D0, Links, Xs, Ys, Zs) :-
    prefix(D0, "x", Kx, _), prefix(D0, "y", Ky, _), prefix(D0, "z", Kz, _),
    insert(D0, Kx, Xs, D1), insert(D1, Ky, Ys, D2), insert(D2, Kz, Zs, D3),
    mix(D3, Links), prefix(D3, "z", _, Zs).
mix(_, []).
mix(D0, [C1-C2-O-C3|Rest]) :-
    rb_lookup(C1, X1, D0), rb_lookup(C2, X2, D0), rb_lookup(C3, X3, D0),
    ops(O, X1, X2, X3),
    mix(D0, Rest).

solve(In, Part1) :-
    read_file_to_string(In, S, []),
    re_foldl([_{0:_,c:C,n:N}, [C-N|V0], V0]>>true, "(?<c>[a-z0-9]+): (?<n_I>[01])",S,Ds0,[],[]),
    keysort(Ds0, Ds),
    re_foldl([_{0:_,c1:C1,o:O,c2:C2,c3:C3}, [C1-C2-O-C3|V0], V0]>>true,
	     "(?<c1>[a-z0-9]+) (?<o>AND|OR|XOR) (?<c2>[a-z0-9]+) -> (?<c3>[a-z0-9]+)",
	     S,Ls,[],[]),
    maplist([_-_-_-K, K]>>true, Ls, EndKeys0), sort(EndKeys0, EndKeys),
    rb_empty(Empty),
    foldl([K, V0, V]>>rb_insert(V0,K,_,V), EndKeys, Empty, Ends),
    foldl([K-X, V0, V]>>rb_insert(V0,K,X,V), Ds, Ends, Defs),
    prefix(Defs, "x", _, DefXs), prefix(Defs, "y", _, DefYs),
    mix(Defs, Ls, DefXs, DefYs, Zs), to_int(Zs, Part1).
