
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

record(_N-_H-_J-_K-_L).

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
    risk_level(Points, Level),
    format("level: ~I~n", [Level]).
