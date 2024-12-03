mul_acc(_{0:_,2:_,op:"mul",x:X,y:Y}, V0, V1) :- V1 is V0+(X*Y).
mul_acc(_, V0, V0).

mul_do_acc(_{0:_,2:_,op:"mul",x:X,y:Y}, A-V0, A-V1) :- V1 is V0+A*(X*Y).
mul_do_acc(_{0:_, op:"do"}, _-V0, 1-V0).
mul_do_acc(_{0:_, op:"don't"}, _-V0, 0-V0).

solve(In, Part1, Part2) :-
    Regex = "(?<op>mul|do|don't)\\(((?<x_I>\\d+),(?<y_I>\\d+))?\\)",    
    read_file_to_string(In, S, []),
    re_foldl(mul_acc, Regex, S, 0, Part1, []),
    re_foldl(mul_do_acc, Regex, S, 1-0, _-Part2, []).
