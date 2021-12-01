
increased_to(N, M) :- N < M.

sonar_sweep([_], Acc, Acc).
sonar_sweep([First,Second|Rest], Acc, Count) :-
    increased_to(First, Second),
    !,
    Acc1 is Acc + 1,
    sonar_sweep([Second|Rest], Acc1, Count).
sonar_sweep([_,Second|Rest], Acc, Count) :-
    sonar_sweep([Second|Rest], Acc, Count).

sonar_sweep([], 0) :-
    !.
sonar_sweep(List, Count) :-
    is_list(List),
    !,
    sonar_sweep(List, 0, Count).
sonar_sweep(FilePath, Count) :-
    atom(FilePath),
    !,
    csv_read_file(FilePath, RawList),
    collect_list(RawList, List),
    sonar_sweep(List, Count).

collect_list([], []).
collect_list([row(Number)|Rest], [Number|Rest1]) :-
    collect_list(Rest, Rest1).

?- sonar_sweep('../inputs/day1_input.txt', 1711).
