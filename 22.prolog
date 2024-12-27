:- use_module([library(yall)]).
:- set_prolog_flag(stack_limit, 2_147_483_648).

secret(Seed, Step3) :-
    Step1 is (Seed xor (Seed * 64)) mod 16777216,
    Step2 is (Step1 xor (Step1 // 32)) mod 16777216,
    Step3 is (Step2 xor (Step2 * 2048)) mod 16777216.

lazy_secret(Seed, Next, Next) :- !, secret(Seed, Next).

lazy_diffs([A,B,C,D,E|T], [B,C,D,E|T], [I,J,K,L,X]) :-
    !, maplist([O-P,M]>>(M is O mod 10 - P mod 10), [B-A,C-B,D-C,E-D,E-0], [I,J,K,L,X]).

tree(LazyDifs, Len, T) :-
    length(Difs, Len), append(Difs, _, LazyDifs),
    rb_empty(T0), foldl([[A,B,C,D,X],V0,V]>>(rb_insert_new(V0,[A,B,C,D],X,V),!;V=V0),Difs,T0,T).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []),
    re_foldl([_{0:N},[N|V0],V0]>>true, "\\d+"/t, S, Ns, [], []),    
    foldl([N,V0,V]>>(lazy_list(lazy_secret,N,L), nth1(2000,L,X), V is V0+X), Ns, 0, Part1),
    maplist([N,Out]>>(lazy_list(lazy_secret, N, L), lazy_list(lazy_diffs,  L, L2),
		      tree(L2, 1996, Out)), Ns, Dicts),
    rb_empty(Combined0),
    foldl([D,V0,V]>>(
	      rb_visit(D, Pairs),
	      foldl([X-Y,U0,U]>>(
			(rb_lookup(X,T0,U0), T is T0+Y, rb_insert(U0,X,T,U),!; rb_insert(U0,X,Y,U))
		    ), Pairs, V0, V)
	  ), Dicts, Combined0, Combined),
    rb_visit(Combined, Pairs2), foldl([_-Y,V0,V]>>(Y>V0->V=Y;V=V0), Pairs2, -9999, Part2).
