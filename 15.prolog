:- use_module([library(clpfd), library(yall)]).

coo(Idx0, Idx, X1-Y1) :-
    d(cols, Cols), d(rows, Rows),
    X0 #= mod(Idx0, Cols), Y0 #= Idx0 // Cols,
    X #= X0 + X1, Y #= Y0 + Y1,
    Cols #> X, Rows #> Y, X #>= 0, Y #>= 0,
    Idx #= X + Y * Cols.

update(M0,X,Y,M) :- get_assoc(X,M0,V), put_assoc(X,M0,'.',M1), put_assoc(Y,M1,V,M).

move(M0,X,_,_) :- \+ (get_assoc(X,M0,V), memberchk(V,['@','O','[',']'])), !, false.
move(M0,X,Y,M) :- get_assoc(X,M0,V), (V='@';V='O'), get_assoc(Y,M0,'.'), !, update(M0,X,Y,M). 
move(M0,X,Y,M) :- get_assoc(X,M0,V), (V='@';V='O'),
		  coo(X,Y,IJ), coo(Y,X1,IJ), !, move(M0,Y,X1,M1), update(M1,X,Y,M).
move(M0,X,Y,M) :- (get_assoc(X,M0,'['), coo(X,X2,1-0), get_assoc(X2,M0,']');
		   get_assoc(X,M0,']'), coo(X,X2,-1-0),get_assoc(X2,M0,'[')),
		  coo(X,Y,IJ), coo(Y,X1,IJ), coo(X2,Y2,IJ), coo(Y2,X3,IJ),
		  (get_assoc(Y2,M0,'.')->M1=M0;move(M0,Y2,X3,M1)),
		  update(M1,X2,Y2,M2),
		  (get_assoc(Y,M2,'.')->M3=M2;move(M2,Y,X1,M3)),
		  !,  update(M3,X,Y,M).

big_map([X0|Xs], Acc, Out) :-
    (X0='\n', X='\n'; X0='#', X=['#','#']; X0='O', X=['[',']'];
     X0='.', X=['.','.']; X0='@', X=['@','.']),
    !, big_map(Xs, [X|Acc], Out).
big_map([], Acc, Out) :- reverse(Acc, Out1), flatten(Out1, Out).

get_map(Cs0, Cs, Cols, Rows) :-
    nth0(Cols,Cs0, '\n'), !, exclude(=('\n'), Cs0, Cs1),
    findall(I-V, nth0(I, Cs1, V), CsIdx), list_to_assoc(CsIdx, Cs),
    length(Cs0, N), Rows is N//Cols.

commit_moves(Dir,Cs,CsLast) :-
    foldl([M,V0,V]>>(
	      nth1(I, ["^","<",">","v"], M), nth1(I, [0-(-1),-1-0,1-0,0-1], XY),
	      gen_assoc(A, V0, '@'), coo(A, B, XY), (move(V0, A, B, V),!;V=V0)
	  ), Dir, Cs, CsLast).

solve(In, Part1, Part2) :-
    read_file_to_string(In, Str, []), re_split("\\n\\n", Str, [MapStr,_,DirStr],[]),
    re_foldl([_{0:X}, [X|V0], V0]>>true, "[<v>^]", DirStr, Dir, [], []),
    string_chars(MapStr, Cs0), get_map(Cs0, Cs, Cols, Rows),
    retractall(d(_,_)), asserta(d(rows, Rows)), asserta(d(cols, Cols)),
    commit_moves(Dir, Cs, CsLast),
    aggregate_all(sum(100*Y+X),(gen_assoc(I,CsLast,'O'), coo(0,I,X-Y)), Part1),
    big_map(Cs0, [], CsBig0), get_map(CsBig0, CsBig, ColsBig, RowsBig),
    retractall(d(_,_)), asserta(d(rows, RowsBig)), asserta(d(cols, ColsBig)),
    commit_moves(Dir, CsBig, CsBigLast), !,
    aggregate_all(sum(100*Y+X),(gen_assoc(I,CsBigLast,'['), coo(0,I,X-Y)), Part2).
