
%% -- Data -----------------------------------
file_path('../inputs/day9_input.txt').

data(Inputs) :-
    file_path(FilePath),
    csv_read_file(FilePath, Rows, [convert(false)]),
    input_atom_list(Rows, Inputs).

input_atom_list([], []).
input_atom_list([row(Atom)|Rows], [Atom|Inputs]) :-
    input_atom_list(Rows, Inputs).

%% --- Day 9: Smoke Basin ---
%% These caves seem to be lava tubes. Parts are even still volcanically active; small hydrothermal vents release smoke into the caves that slowly settles like rain.

%% If you can model how the smoke flows through the caves, you might be able to avoid it and be that much safer. The submarine generates a heightmap of the floor of the nearby caves for you (your puzzle input).

%% Smoke flows to the lowest point of the area it's in. For example, consider the following heightmap:

%% 2199943210
%% 3987894921
%% 9856789892
%% 8767896789
%% 9899965678
%% Each number corresponds to the height of a particular location, where 9 is the highest and 0 is the lowest a location can be.

%% Your first goal is to find the low points - the locations that are lower than any of its adjacent locations. Most locations have four adjacent locations (up, down, left, and right); locations on the edge or corner of the map have three or two adjacent locations, respectively. (Diagonal locations do not count as adjacent.)

%% In the above example, there are four low points, all highlighted: two are in the first row (a 1 and a 0), one is in the third row (a 5), and one is in the bottom row (also a 5). All other locations on the heightmap have some lower adjacent location, and so are not low points.

%% The risk level of a low point is 1 plus its height. In the above example, the risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the heightmap is therefore 15.

%% Find all of the low points on your heightmap. What is the sum of the risk levels of all low points on your heightmap?

sealed_rows([Term|Terms], [Record|Records]) :-
    bare_rows([Term|Terms], [Record|Records]),
    findall(N-H-J-K-L, (member(N-H-J-K-L,Record),nil=K), Record),
    findall([N-H-J-K-L|Rest], (member([N-H-J-K-L|Rest],[Record|Records]),nil=H), [Record|Records]).

bare_rows([Term], [Records]) :-
    row(Term, Records_1),
    findall(N-H-nil-K-L, member(N-H-_J-K-L,Records_1), Records).
bare_rows([T1,T2|Rest], [Row1,Row2|Rows]) :-
    row(T1, Row1),
    bare_rows([T2|Rest], [Row2|Rows]),
    interweave(Row1, Row2).

interweave([], []).
interweave([N1-_H1-N2-_K1-_L1|Rest1],
           [N2-_H2-_J2-N1-_L2|Rest2]) :-
    interweave(Rest1, Rest2).

row(Atom, Records) :-
    atom(Atom),
    !,
    atom_chars(Atom, List),
    row(List, Records).
row([A], [Rec]) :-
    record(Rec),
    atom_number(A, N),
    N-_H-_J-_K-nil = Rec.
row([A1,A2|T], [Rec1,Rec2|Records]) :-
    atom_number(A1, N1),
    atom_number(A2, N2),
    record(Rec1),
    N1-_H1-_J1-_K1-N2 = Rec1,
    row([A2|T], [Rec2|Records]),
    N2-N1-_J2-_K2-_L2 = Rec2.

record(_N-_H-_J-_K-_L).     % As a risk point.

risk_points([Record|Records], Points) :-
    is_list(Record),
    !,
    flatten([Record|Records], List),
    risk_points(List, Points).
risk_points(List, Points) :-
    findall(N-H-J-K-L, (member(N-H-J-K-L, List),
                        lower_than(N, H),
                        lower_than(N, J),
                        lower_than(N, K),
                        lower_than(N, L)
                       ), Points).

lower_than(_, nil) :-
    !.
lower_than(L, H) :-
    H > L.

risk_level(Points, N) :-
    findall(N, member(N-_H-_J-_K-_L,Points), Levels),
    length(Levels, N1),
    sumlist(Levels, N2),
    N is N1 + N2.

solution :-
    data(Inputs),
    sealed_rows(Inputs, Rows),
    risk_points(Rows, Points),
    format("~p~n",[Points]),
    risk_level(Points, Level),
    format("level: ~I~n", [Level]).

%% --- Part Two ---
%% Next, you need to find the largest basins so you know what areas are most important to avoid.

%% A basin is all locations that eventually flow downward to a single low point. Therefore, every low point has a basin, although some basins are very small. Locations of height 9 do not count as being in any basin, and all other locations will always be part of exactly one basin.

%% The size of a basin is the number of locations within the basin, including the low point. The example above has four basins.

%% The top-left basin, size 3:

%% 2199943210
%% 3987894921
%% 9856789892
%% 8767896789
%% 9899965678
%% The top-right basin, size 9:

%% 2199943210
%% 3987894921
%% 9856789892
%% 8767896789
%% 9899965678
%% The middle basin, size 14:

%% 2199943210
%% 3987894921
%% 9856789892
%% 8767896789
%% 9899965678
%% The bottom-right basin, size 9:

%% 2199943210
%% 3987894921
%% 9856789892
%% 8767896789
%% 9899965678
%% Find the three largest basins and multiply their sizes together. In the above example, this is 9 * 14 * 9 = 1134.

%% What do you get if you multiply together the sizes of the three largest basins?

basins([Atom], [Basins], Next_id) :-
    line_basins(Atom, Basins),
    number(Basins, 1, Next_id).
    %findall(I-C-T, (member(I-C-T,Basins),(var(T),T=C;not(var(T)))), Basins).
basins([A1,A2|Atoms], [B1,B2|Basins], Next_id) :-
    basins([A2|Atoms], [B2|Basins], Next_id_1),
    line_basins(A1, B1),
    number(B1, Next_id_1, Next_id),
    format("~p~n~p~n",[A1,B1]),
    migrate([0,0], B2, B1),
    format("~p~n~p~n",[B1,B2]).

number([], Next_id, Next_id).
number([I-_C-_T|List], N, Next_id) :-
    var(I),
    !,
    I = N,
    N1 is N + 1,
    number(List, N1, Next_id).
number([_I-_C-_T|List], N, Next_id) :-
    number(List, N, Next_id).

migrate(_Run_sums, [], []).
migrate([N,S], [I1-C1-T1|Rest1], List2) :-
    I1 \= nil,
    not(var(T1)),
    !,
    N1 is N + T1,
    migrate([N1,S], [I1-C1-_T1|Rest1], List2).
migrate([N,S], List1, [I2-C2-T2|Rest2]) :-
    I2 \= nil,
    not(var(T2)),
    !,
    S1 is S + T2,
    migrate([N,S1], List1, [I2-C2-_T2|Rest2]).
migrate([N,S], [I1-C1-T1|Rest1], [I2-C2-T2|Rest2]) :-
    (var(I1); I1 \= nil),
    (var(I2); I2 \= nil),
    !,
    format("c1: ~p ~p ~p~n", [[N,S], [I1-C1-T1|Rest1], [I2-C2-T2|Rest2]]),
    M is min(C1, C2),
    I2 = I1,
    (C1 =:= C2,
     !,
     T1 is N + C1,
     T2 is T1 + S + C2,
     migrate([0,0], Rest1, Rest2);
     M =:= C1,
     !,
     T1 is N + C1,
     S1 is T1 + S + M,
     C3 is C2 - M,
     migrate([0,S1], Rest1, [I2-C3-T2|Rest2]);
     T2 is S + C2,
     N1 is N + M + T2,
     C3 is C1 - M,
     migrate([N1,0], [I1-C3-T1|Rest1], Rest2)
    ).
migrate([N,_S], [I1-C1-T1|Rest1], [nil-C2-nil|Rest2]) :-
    (var(I1); I1 \= nil),
    !,
    format("c2: ~p ~p ~p~n", [[N,_S], [I1-C1-T1|Rest1], [nil-C2-nil|Rest2]]),
    M is min(C1, C2),
    (C1 =:= C2,
     !,
     T1 is N + C1,
     migrate([0,0], Rest1, Rest2);
     M =:= C1,
     !,
     T1 is N + C1,
     C3 is C2 - M,
     migrate([0,0], Rest1, [nil-C3-nil|Rest2]);
     N1 is N + M,
     C3 is C1 - M,
     migrate([N1,0], [I1-C3-T1|Rest1], Rest2)
    ).
migrate([_N,S], [nil-C1-nil|Rest1], [I2-C2-T2|Rest2]) :-
    (var(I2); I2 \= nil),
    !,
    format("c3: ~p ~p ~p~n", [[_N,S], [nil-C1-nil|Rest1], [I2-C2-T2|Rest2]]),
    M is min(C1, C2),
    (C1 =:= C2,
     !,
     T2 is S + C2,
     migrate([0,0], Rest1, Rest2);
     M =:= C1,
     !,
     S1 is S + M,
     C3 is C2 - M,
     migrate([0,S1], Rest1, [I2-C3-T2|Rest2]);
     T2 is S + C2,
     C3 is C1 - M,
     migrate([0,0], [nil-C3-nil|Rest1], Rest2)
    ).
migrate([_N,_S], [nil-C1-nil|Rest1], [nil-C2-nil|Rest2]) :-
    format("c4: ~p ~p ~p~n", [[_N,_S], [nil-C1-nil|Rest1], [nil-C2-nil|Rest2]]),
    M is min(C1, C2),
    (C1 =:= C2,
     !,
     migrate([0,0], Rest1, Rest2);
     M =:= C1,
     !,
     C3 is C2 - M,
     migrate([0,0], Rest1, [nil-C3-nil|Rest2]);
     C3 is C1 - M,
     migrate([0,0], [nil-C3-nil|Rest1], Rest2)
    ).

line_basins(Atom, Basins) :-
    atom(Atom),
    !,
    atom_chars(Atom, List),
    separate_9(List, List_1),
    findall(I-C-T, (member(L, List_1),
                    basin_line(I-C-T),
                    length(L, C),
                    (['9'|_] = L,
                     I = nil,
                     T = nil;
                     ['9'|_] \= L
                    )
                   ), Basins).

basin_line(_Id-_Count-_Total).

color_map(Num, [Atom], [List], Next_num) :-
    !,
    color_basins(Num, Atom, List, Next_num).
color_map(Num, [A1,A2|Atoms], [L1,L2|Lists], Next_num) :-
    color_map(Num, [A2|Atoms], [L2|Lists], Num_1),
    atom_chars(A1, List),
    findall(E, (member(X,List),(X=='9',E='9';X\='9')), List_1),
    infect_by(List_1, L2),
    spread(List_1),
    find_conflict(List_1, Conflict_set),
    color_basins(Num_1, List_1, L1, Next_num).

infect_by([], []) :-
    !.
infect_by([X|Rest], [Y|List]) :-
    var(X),
    Y \= '9',
    !,
    Y = X,
    infect_by(Rest, List).
infect_by([_|Rest], ['9'|List]) :-
    !,
    infect_by(Rest, List).
infect_by(['9'|Rest], [_|List]) :-
    !,
    infect_by(Rest, List).

spread([_]).
spread([A1,A2|Rest]) :-
    bind(A1,A2),
    bind(A2,A1),
    spread([A2|Rest]).

bind(X, Y) :-
    var(X),
    ground(Y),
    Y \= '9',
    !,
    X = Y.
bind(_, _).

find_conflict([_], []).
find_conflict([X,Y|Rest], [X-Y|List]) :-
    ground(X),
    ground(Y),
    X \= '9',
    Y \= '9',
    X \= Y,
    !,
    find_conflict([Y|Rest], List).
find_conflict([_,Y|Rest], List) :-
    find_conflict([Y|Rest], List).

color_basins(Num, Atom, List, Next_num) :-
    atom(Atom),
    !,
    atom_chars(Atom, List_1),
    findall(E, (member(X,List_1),(X='9',E='9';X\='9')), List_2),
    color_basins(Num, List_2, List, Next_num).
color_basins(Num, [], [], Next_num) :-
    Next_num is Num + 1.
color_basins(Num, [X|Rest], [X|List], Next_num) :-
    var(X),
    !,
    X = Num,
    color_basins(Num, Rest, List, Next_num).
color_basins(Num, ['9'|Rest], ['9'|List], Next_num) :-
    !,
    N1 is Num + 1,
    color_basins(N1, Rest, List, Next_num).
color_basins(Num, [X|Rest], [X|List], Next_num) :-
    ground(X),
    !,
    color_basins(Num, Rest, List, Next_num).

separate_9([A], [[A]]).
separate_9([A,A|Atoms], [[A|Atoms1]|Atoms2]) :-
    A = '9',
    !,
    separate_9([A|Atoms], [Atoms1|Atoms2]).
separate_9(['9'|Atoms], [['9']|Atoms1]) :-
    !,
    separate_9(Atoms, Atoms1).
separate_9([A,'9'|Atoms], [[A]|Atoms1]) :-
    !,
    separate_9(['9'|Atoms], Atoms1).
separate_9([A,B|Atoms], [[A|Atoms1]|Atoms2]) :-
    separate_9([B|Atoms], [Atoms1|Atoms2]).

sort_pred('<', A1-F1, A2-F2) :-
    F1 > F2.
sort_pred('>', A1-F1, A2-F2) :-
    F1 < F2.
sort_pred('=', A1-F1, A2-F2) :-
    F1 =:= F2.

solution_2 :-
    data(Input),
    color_map(0, Input, Basins, _),
    flatten(Basins, List),

    findall(C , (member(B,Basins),find_conflict(B,C)), Conflict_set),
    flatten(Conflict_set, Cfset),
    format("Conflict colors: ~p~n", [Cfset]),

    %findall(A-X-B, (member(X-B,Cfset),member(A-X,Cfset)), Again),
    %format("~nAgain set: ~p~n~n", [Again]),

    %format("~p~n", [List]),
    findall(E, (member(E,List),number(E)), Result_X),
    %format("~p~n", [Result_X]),
    findall(E, (member(X,List),
                number(X),
                (member(X-R,Cfset),
                 E=R;
                 not(member(X-_,Cfset))
                 ,E=X)), Result),
    %format("~p~n", [Result]),


    findall(E, (member(X,Result),
                number(X),
                (member(X-R,Cfset),
                 E=R;
                 not(member(X-_,Cfset))
                 ,E=X)), R2),

    findall(E, (member(X,R2),
                number(X),
                (member(X-R,Cfset),
                 E=R;
                 not(member(X-_,Cfset))
                 ,E=X)), R3),


    findall(E, (member(X,R3),
                number(X),
                (member(X-R,Cfset),
                 E=R;
                 not(member(X-_,Cfset))
                 ,E=X)), R4),
    findall(E, (member(X,R4),
                number(X),
                (member(X-R,Cfset),
                 E=R;
                 not(member(X-_,Cfset))
                 ,E=X)), R5),

    findall(E, (member(X,R5),
                number(X),
                (member(X-R,Cfset),
                 E=R;
                 not(member(X-_,Cfset))
                 ,E=X)), R6),

    msort(R6, Result_1),
    clumped(Result_1, Result_2),

    findall(F-N, member(N-F,Result_2), Result_3),

    msort(Result_3, Result_4),
    reverse(Result_4, Result_5),

    format("Freq-Basin. list: ~p~n", [Result_5]),


    %predsort(sort_pred, Result_2, Result_3),
    %format("~n ~p~n", [Result_3]),

    [N1-_, N2-_, N3-_|_] = Result_5,

    N is N1 * N2 * N3,
    format("Largest three counts: ~I ~I ~I~n", [N1, N2, N3]),
    format("Product of them: ~I~n", [N]).

    %foreach(member(A,Input), format("~p~n",[A])),
    %foreach(member(B,Basins), format("~p~n",[B])),
    %findall(C , (member(B,Basins),find_conflict(B,C)), Conflict_set),
    %flatten(Conflict_set, Cfset),
    %format("~p~n", [Cfset]).
