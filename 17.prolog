:- use_module([library(clpfd), library(yall)]).

combo(Code,_,_,_,Code)  :- Code #>= 0, 4 #> Code.
combo(Code0,A,B,C,Code) :- Code0 #> 3, I #= 1+mod(Code0,4), element(I,[A,B,C],Code).

execute([0,Y0|Rest],P,A0,B,C,XX,O) :- combo(Y0,A0,B,C,Y), A #= A0 // (2^Y), execute(Rest,P,A,B,C,XX,O).
execute([1,Y|Rest],P,A,B0,C,XX,O)  :- B #= B0 xor Y, execute(Rest,P,A,B,C,XX,O).
execute([2,Y0|Rest],P,A,B0,C,XX,O) :- combo(Y0,A,B0,C,Y), B #= mod(Y,8), execute(Rest,P,A,B,C,XX,O).
execute([3,_|Rest],P,0,B,C,XX,O)   :- execute(Rest,P,0,B,C,XX,O).
execute([3,Y|_],P0,A,B,C,XX,O)     :- length(X,Y), append(X,P,P0), execute(P,P0,A,B,C,XX,O).
execute([4,_|Rest],P,A,B0,C,XX,O)  :- B #= B0 xor C, execute(Rest, P,A,B,C,XX,O).
execute([5,Y0|Rest],P,A,B,C,XX,O)  :- combo(Y0,A,B,C,Y), X #= Y mod 8, execute(Rest, P,A,B,C,[X|XX],O).
execute([6,Y0|Rest],P,A,B0,C,XX,O) :- combo(Y0,A,B0,C,Y), B #= A // (2^Y), execute(Rest,P,A,B,C,XX,O).
execute([7,Y0|Rest],P,A,B,C0,XX,O) :- combo(Y0,A,B,C0,Y), C #= A // (2^Y), execute(Rest,P,A,B,C,XX,O).
execute([], _,_,_,_,O0,O) :- reverse(O0,O).

solve(In, Part1, Part2) :-
    read_file_to_string(In, S, []),
    re_foldl([_{0:N},[N|V0],V0]>>true, "\\d+"/t, S, [A,B,C|Prog], [], []),
    execute(Prog,Prog,A,B,C,[],Part1),
    Part2 #> 0, execute(Prog,Prog,Part2,B,C,[],Prog), labeling([bisect],[Part2]).
