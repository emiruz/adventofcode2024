mul_do_acc(_{0:_,2:_,op:"mul",x:X,y:Y}, A-V0-V1, A-V2-V3) :-
    V2 is V0+(X*Y), V3 is V1+A*(X*Y).
mul_do_acc(_{0:_, op:"do"}, _-V0-V1, 1-V0-V1).
mul_do_acc(_{0:_, op:"don't"}, _-V0-V1, 0-V0-V1).

solve(In, Part1, Part2) :-
    Regex = "(?<op>mul|do|don't)\\(((?<x_I>\\d+),(?<y_I>\\d+))?\\)",    
    read_file_to_string(In, S, []),
    re_foldl(mul_do_acc, Regex, S, 1-0-0, _-Part1-Part2, []).
