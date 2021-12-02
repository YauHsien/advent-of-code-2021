
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

%% -- Part2 ----------------------------

sliding3([First,Second,Third|Rest], Acc, nil, Count) :-
    !,
    Sum is First + Second + Third,
    sliding3([Second,Third|Rest], Acc, Sum, Count).
sliding3([First,Second,Third|Rest], Acc, PrevSum, Count) :-
    Sum is First + Second + Third,
    Sum > PrevSum,
    !,
    Acc1 is Acc + 1,
    sliding3([Second,Third|Rest], Acc1, Sum, Count).
sliding3([First,Second,Third|Rest], Acc, _, Count) :-
    Sum is First + Second + Third,
    sliding3([Second,Third|Rest], Acc, Sum, Count).
sliding3(List, Count, _, Count) :-
    is_list(List),
    ([_,_] = List, !;
     [_] = List, !;
     [] = List, !).

sliding3_sweep(FilePath, Count) :-
    atom(FilePath),
    !,
    csv_read_file(FilePath, RawList),
    collect_list(RawList, List),
    sliding3(List, 0, nil, Count).

%% -- Solutions ------------------------
?- sonar_sweep('../inputs/day1_input.txt', 1711).
?- sliding3_sweep('../inputs/day1_input.txt', 1743).
