:- use_module([library(yall)]).

:- table n_coo/2, d_coo/2, path/4.

n_coo(N,J-K) :-
    nth0(I,['A','0','1','2','3','4','5','6','7','8','9'],N),
    nth0(I,[2,1,0,1,2,0,1,2,0,1,2],J),
    nth0(I,[3,3,2,2,2,1,1,1,0,0,0],K).

d_coo(D,J-K) :-
    nth0(I,['A','^','<','v','>'],D), nth0(I,[2,1,0,1,2],J), nth0(I,[0,0,1,1,1],K).

manhattan([X,Y|Xs], Coo, A0, Out) :-
    call(Coo, X, I-J), call(Coo, Y, K-L),
    Dist is abs(I-K) + abs(J-L),
    A is Dist + A0, !,
    manhattan([Y|Xs], Coo, A, Out).
manhattan([_], _, A, A).

path(E, E, _, _, Final, Final).
path(S, E, Coo, Viz, Acc0, Final) :-
    call(Coo,S,X-Y), nth0(Idx,[1-0,-1-0,0-1,0-(-1)],I-J), A is X+I, B is Y+J,
    nth0(Idx, ['>','<','v','^'], D), call(Coo,N,A-B), \+ memberchk(N,Viz),
    append(Acc0, [D], Acc),
    path(N,E,Coo,[S|Viz],Acc,Final).
path(S, E, Coo, L) :-
    findall(N-P, (path(S,E,Coo,[],[],P), manhattan(P, d_coo, 0, N)), Ps),
    (sort(Ps,[_-L0|_]),!;L0=[]), append(L0,['A'],L).

code(_, [_]-[], _, _) :- !, false.
code(_, Xs-[H|T], Xs-T, H) :- !.
code(Coo, [X,Y|Xs]-[], [Y|Xs]-T, H) :- !, path(X,Y,Coo,[H|T]).
code(Coo, [_,Y|Xs]-[], State, H) :- !, code(Coo, [Y|Xs]-[], State, H).
code(Xs, Coo, Out) :- lazy_list(code(Coo), Xs-[], Out).

pipe(Out, _, 0, Out) :- !.
pipe(In, N, M0, Out) :-
    (N=:=M0->Coo=n_coo; Coo=d_coo),
    !, code(['A'|In], Coo, In2), M is M0-1, pipe(In2, N, M, Out).
pipe(In, N, Final) :-
    pipe(In, N, N, Out),
    foldl([_,V0,V]>>(V is V0+1), Out, 0, Final).

solve(In, Part1) :-
    read_file_to_string(In, S, []), split_string(S,'\n','',Ss0), exclude(=(""), Ss0, Ss),
    aggregate_all(sum(N1*N2),
		  (member(X,Ss), string_chars(X, Xs), re_replace('[^0-9]','',X,X0),
		   pipe(Xs,3,N1), atom_number(X0, N2)), Part1).
