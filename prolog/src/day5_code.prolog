%% --- Day 5: Hydrothermal Venture ---
%% You come across a field of hydrothermal vents on the ocean floor! These vents constantly produce large, opaque clouds, so it would be best to avoid them if possible.
%
%% They tend to form in lines; the submarine helpfully produces a list of nearby lines of vents (your puzzle input) for you to review. For example:
%
%% 0,9 -> 5,9
%% 8,0 -> 0,8
%% 9,4 -> 3,4
%% 2,2 -> 2,1
%% 7,0 -> 7,4
%% 6,4 -> 2,0
%% 0,9 -> 2,9
%% 3,4 -> 1,4
%% 0,0 -> 8,8
%% 5,5 -> 8,2
%% Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 where x1,y1 are the coordinates of one end the line segment and x2,y2 are the coordinates of the other end. These line segments include the points at both ends. In other words:
%
%% An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
%% An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
%% For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.
%
%% So, the horizontal and vertical lines from the above list would produce the following diagram:
%
%% .......1..
%% ..1....1..
%% ..1....1..
%% .......1..
%% .112111211
%% ..........
%% ..........
%% ..........
%% ..........
%% 222111....
%% In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9. Each position is shown as the number of lines which cover that point or . if no line covers that point. The top-left pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.
%
%% To avoid the most dangerous areas, you need to determine the number of points where at least two lines overlap. In the above example, this is anywhere in the diagram with a 2 or larger - a total of 5 points.
%
%% Consider only horizontal and vertical lines. At how many points do at least two lines overlap?

atom_rule(Atom, Rule) :-
    atom_chars(Atom, Chars),
    chars_rule(Chars, [[]], Rule).

chars_rule([], [Chars|Rest], Rule) :-
    reverse_chars_number(Chars, Number),
    reverse([Number|Rest], Rule).
chars_rule([','|Rest], [Chars|Rest1], Rule) :-
    !,
    reverse_chars_number(Chars, Number),
    chars_rule(Rest, [[],Number|Rest1], Rule).
chars_rule([' ','-','>',' '|Rest], [Chars|Rest1], Rule) :-
    !,
    reverse_chars_number(Chars, Number),
    chars_rule(Rest, [[],Number|Rest1], Rule).
chars_rule([C|Rest], [Chars|Rest1], Rule) :-
    chars_rule(Rest, [[C|Chars]|Rest1], Rule).

reverse_chars_number(Chars, Number) :-
    reverse(Chars, Chars1),
    atom_chars(Atom, Chars1),
    atom_number(Atom, Number).

file_path('../inputs/day5_input.txt').

rows_commands([], _Lines, Points, Points).
rows_commands([row(Atom)|Rest], Acc_lines, Acc_points, Points) :-
    atom_rule(Atom, Line),
    (([X,_,X,_] = Line,
      !;
      [_,Y,_,Y] = Line
     ),
     !,
     pair_lines(Line, Acc_lines, Points1),
     append(Points1, Acc_points, Ps),
     (setof(P, member(P,Ps), Acc_points1),
      !,
      rows_commands(Rest, [Line|Acc_lines], Acc_points1, Points);
      rows_commands(Rest, [Line|Acc_lines], Acc_points, Points)
     );
     %format("ignore: ~p~n", [Line]),
     rows_commands(Rest, Acc_lines, Acc_points, Points)
    ).

pair_lines(_Line, [], []).
pair_lines(Line, [Line1|Lines], Points) :-
    is_list(Line1),
    !,
    pair_lines(Line, Line1, Points1),
    %format("~p ~p ~p~n", [Line,Line1, Points1]),
    pair_lines(Line, Lines, Points2),
    append(Points1, Points2, Points3),
    (setof(E, member(E,Points3), Points),
     !;
     Points = []
    ).
pair_lines([X,Y1,X,Y2], [X,Y3,X,Y4], Points) :-
    redundant_sequence([Y1,Y2], [Y3,Y4], Sequence),
    (bagof((X,Y), member(Y,Sequence), Points);
     Points = []
    ),
    !.
pair_lines([X1,Y,X2,Y], [X3,Y,X4,Y], Points) :-
    redundant_sequence([X1,X2], [X3,X4], Sequence),
    (bagof((X,Y), member(X,Sequence), Points);
     Points = []
    ),
    !.
pair_lines([X,Y1,X,Y2], [X1,Y,X2,Y], [(X,Y)]) :-
    normal_seq([X1,X2], [X3,X4]),
    between(X3, X4, X),
    normal_seq([Y1,Y2], [Y3,Y4]),
    between(Y3, Y4, Y),
    !.
pair_lines([X1,Y,X2,Y], [X,Y1,X,Y2], [(X,Y)]) :-
    normal_seq([X1,X2], [X3,X4]),
    between(X3, X4, X),
    normal_seq([Y1,Y2], [Y3,Y4]),
    between(Y3, Y4, Y),
    !.
pair_lines(_Line1, _Line2, []).

redundant_sequence([A,B], [C,D], Sequence) :-
    normal_seq([A,B], [E,F]),
    normal_seq([C,D], [G,H]),
    N is min(F, H),
    (between(E, F, G),
     !,
     bagof(X, between(G,N,X), Sequence),
     %format("sequence: ~p ~p ~p~n", [G,N,Sequence]),
     true;
     between(G, H, E),
     !,
     bagof(X, between(E,N,X), Sequence),
     %format("sequence: ~p ~p ~p~n", [E,N,Sequence]),
     true;
     %format("forget: ~p ~p~n", [[E,F], [G,H]]),
     Sequence = []
    ).

normal_seq([A,B], [C,D]) :-
    C is min(A, B),
    D is max(A, B).

%% -- Solution --------------------------------
:- file_path(FilePath),
   csv_read_file(FilePath, Rows, [separator(0'$)]),
   rows_commands(Rows, [], [], Points),
   %format("~p~n", [Points]),
   %length(Points, N),
   %format("count: ~p~n", [N]),
   setof(E, member(E,Points), Ps),
   length(Ps, Pn),
   format("count: ~p~n", [Pn]),
   true.

%% --- Part Two ---
%% Unfortunately, considering only horizontal and vertical lines doesn't give you the full picture; you need to also consider diagonal lines.
%
%% Because of the limits of the hydrothermal vent mapping system, the lines in your list will only ever be horizontal, vertical, or a diagonal line at exactly 45 degrees. In other words:
%
%% An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
%% An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.
%% Considering all lines from the above example would now produce the following diagram:
%
%% 1.1....11.
%% .111...2..
%% ..2.1.111.
%% ...1.2.2..
%% .112313211
%% ...1.2....
%% ..1...1...
%% .1.....1..
%% 1.......1.
%% 222111....
%% You still need to determine the number of points where at least two lines overlap. In the above example, this is still anywhere in the diagram with a 2 or larger - now a total of 12 points.
%
%% Consider all of the lines. At how many points do at least two lines overlap?

rows_commands_2([], _Lines, Points, Points).
rows_commands_2([row(Atom)|Rest], Acc_lines, Acc_points, Points) :-
    atom_rule(Atom, Line),
    ((vertical(Line),
      !;
      horizontal(Line),
      !;
      is_diagonal(Line)
     ),
     !,
     pair_lines_2(Line, Acc_lines, Points1),
     append(Points1, Acc_points, Ps),
     (setof(P, member(P,Ps), Acc_points1),
      !,
      rows_commands_2(Rest, [Line|Acc_lines], Acc_points1, Points);
      rows_commands_2(Rest, [Line|Acc_lines], Acc_points, Points)
     );
     %format("ignore: ~p~n", [Line]),
     rows_commands_2(Rest, [Line|Acc_lines], Acc_points, Points)
    ).

pair_lines_2(_Line, [], []).
pair_lines_2(Line, [Line1|Lines], Points) :-
    is_list(Line1),
    !,
    pair_lines_2(Line, Line1, Points1),
    %format("~p ~p ~p~n", [Line,Line1, Points1]),
    pair_lines_2(Line, Lines, Points2),
    append(Points1, Points2, Points3),
    (setof(E, member(E,Points3), Points),
     !;
     Points = []
    ).
pair_lines_2(Line1, Line2, Points) :-
    points(Line1, Points1),
    points(Line2, Points2),
    intersection(Points1, Points2, Points).

cross_point([X1,Y1,X2,Y2], [X,Y]) :-
    points([X1,Y1,X2,Y2], Points),
    member((X,Y), Points).

points([X1,Y1,X2,Y2], Points) :-
    sequence(X1, X2, Xs),
    sequence(Y1, Y2, Ys),
    (is_diagonal([X1,Y1,X2,Y2]),
     !,
     zip(Xs, Ys, Points);
     vertical([X1,Y1,X2,Y2]),
     !,
     bagof((X1,Y), member(Y,Ys), Points);
     horizontal([X1,Y1,X2,Y2]),
     !,
     bagof((X,Y1), member(X,Xs), Points)
    ).

zip([], _, []) :-
    !.
zip(_, [], []).
zip([X|Xs], [Y|Ys], [(X,Y)|List]) :-
    zip(Xs, Ys, List).

sequence(N, M, Sequence) :-
    between(N, M, _),
    !,
    bagof(E, between(N,M,E), Sequence).
sequence(N, M, Sequence) :-
    bagof(E, between(M,N,E), Sequence1),
    reverse(Sequence1, Sequence).

vertical([X,_,X,_]).
horizontal([_,Y,_,Y]).
not_diagonal(List) :-
    vertical(List),
    !.
not_diagonal(List) :-
    horizontal(List).
is_diagonal([X1,Y1,X2,Y2]) :-
    not(not_diagonal([X1,Y1,X2,Y2])),
    normal_seq([X1,X2], [X3,X4]),
    normal_seq([Y1,Y2], [Y3,Y4]),
    N is X4-X3,
    M is Y4-Y3,
    N =:= M.

%% -- Solution --------------------------------------
:- file_path(FilePath),
   csv_read_file(FilePath, Rows, [separator(0'$)]),
   rows_commands_2(Rows, [], [], Points),
   %format("~p~n", [Points]),
   %length(Points, N),
   %format("count: ~p~n", [N]),
   setof(E, member(E,Points), Ps),
   length(Ps, Pn),
   format("count: ~p~n", [Pn]),
   true.
