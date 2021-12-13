
%% -- Data -----------------------------------
file_path('../inputs/day13_input.txt').

data(Inputs) :-
    file_path(FilePath),
    csv_read_file(FilePath, Rows, [convert(false),match_arity(false)]),
    input_atom_list(Rows, Inputs).

input_atom_list([], []-[]).
input_atom_list([row(X,Y)|Rows], [point(N,M)|Points]-Commands) :-
    atom_number(X, N),
    atom_number(Y, M),
    input_atom_list(Rows, Points-Commands).
input_atom_list([row('')|Rows], Inputs) :-
    input_atom_list(Rows, Inputs).
input_atom_list([row(Atom)|Rows], Points-[Term|Commands]) :-
    atom_prefix(Atom, 'fold along '),
    input_term(Atom, Term),
    input_atom_list(Rows, Points-Commands).

input_term(Atom, fold_along(C, N)) :-
    atomic_list_concat(['fold','along',A_1], ' ', Atom),
    atomic_list_concat([C,V], '=', A_1),
    atom_number(V, N).

%% --- Day 13: Transparent Origami ---
%% You reach another volcanically active part of the cave. It would be nice if you could do some kind of thermal imaging so you could tell ahead of time which caves are too hot to safely enter.
%% 
%% Fortunately, the submarine seems to be equipped with a thermal camera! When you activate it, you are greeted with:
%
%% Congratulations on your purchase! To activate this infrared thermal imaging
%% camera system, please enter the code found on page 1 of the manual.
%% Apparently, the Elves have never used this feature. To your surprise, you manage to find the manual; as you go to open it, page 1 falls out. It's a large sheet of transparent paper! The transparent paper is marked with random dots and includes instructions on how to fold it up (your puzzle input). For example:
%
%% 6,10
%% 0,14
%% 9,10
%% 0,3
%% 10,4
%% 4,11
%% 6,0
%% 6,12
%% 4,1
%% 0,13
%% 10,12
%% 3,4
%% 3,0
%% 8,4
%% 1,10
%% 2,14
%% 8,10
%% 9,0
%
%% fold along y=7
%% fold along x=5
%% The first section is a list of dots on the transparent paper. 0,0 represents the top-left coordinate. The first value, x, increases to the right. The second value, y, increases downward. So, the coordinate 3,0 is to the right of 0,0, and the coordinate 0,7 is below 0,0. The coordinates in this example form the following pattern, where # is a dot on the paper and . is an empty, unmarked position:
%
%% ...#..#..#.
%% ....#......
%% ...........
%% #..........
%% ...#....#.#
%% ...........
%% ...........
%% ...........
%% ...........
%% ...........
%% .#....#.##.
%% ....#......
%% ......#...#
%% #..........
%% #.#........
%% Then, there is a list of fold instructions. Each instruction indicates a line on the transparent paper and wants you to fold the paper up (for horizontal y=... lines) or left (for vertical x=... lines). In this example, the first fold instruction is fold along y=7, which designates the line formed by all of the positions where y is 7 (marked here with -):
%
%% ...#..#..#.
%% ....#......
%% ...........
%% #..........
%% ...#....#.#
%% ...........
%% ...........
%% -----------
%% ...........
%% ...........
%% .#....#.##.
%% ....#......
%% ......#...#
%% #..........
%% #.#........
%% Because this is a horizontal line, fold the bottom half up. Some of the dots might end up overlapping after the fold is complete, but dots will never appear exactly on a fold line. The result of doing this fold looks like this:
%
%% #.##..#..#.
%% #...#......
%% ......#...#
%% #...#......
%% .#.#..#.###
%% ...........
%% ...........
%% Now, only 17 dots are visible.
%
%% Notice, for example, the two dots in the bottom left corner before the transparent paper is folded; after the fold is complete, those dots appear in the top left corner (at 0,0 and 0,1). Because the paper is transparent, the dot just below them in the result (at 0,3) remains visible, as it can be seen through the transparent paper.
%
%% Also notice that some dots can end up overlapping; in this case, the dots merge together and become a single dot.
%
%% The second fold instruction is fold along x=5, which indicates this line:
%
%% #.##.|#..#.
%% #...#|.....
%% .....|#...#
%% #...#|.....
%% .#.#.|#.###
%% .....|.....
%% .....|.....
%% Because this is a vertical line, fold left:
%
%% #####
%% #...#
%% #...#
%% #...#
%% #####
%% .....
%% .....
%% The instructions made a square!
%
%% The transparent paper is pretty big, so for now, focus on just completing the first fold. After the first fold in the example above, 17 dots are visible - dots that end up overlapping after the fold is completed count as a single dot.
%
%% How many dots are visible after completing just the first fold instruction on your transparent paper?

%% --- Part Two ---
%% Finish folding the transparent paper according to the instructions. The manual says the code is always eight capital letters.
%
%% What code do you use to activate the infrared thermal imaging camera system?

fold([], Points, Points).
fold([fold_along(A,V)|Commands], Points, Points_1) :-
    sort(Points, Ps_1),
    %draw(Ps_1, along(A,V)),
    show_count(Ps_1),
    format("fold along ~a = ~I:~n", [A,V]),
    fold(along(A,V), Ps_1, Points_2),
    fold(Commands, Points_2, Points_1).
fold(along(x,Xl), Points, Points_1) :-
    findall(point(Xm,Y),
            (member(point(X,Y), Points),
             (X > Xl,
              Xm is 2 * Xl - X;
              X < Xl,
              Xm = X
             )),
            Points_1).
fold(along(y,Yl), Points, Points_1) :-
    findall(point(X,Ym),
            (member(point(X,Y), Points),
             (Y > Yl,
              Ym is 2 * Yl - Y;
              Y < Yl,
              Ym = Y
             )),
            Points_1).

frame(Points, [X,Y]) :-
    findall(X, (member(point(X,_),Points);
                member(h(X,_),Points);
                member(v(X,_),Points)), Xs),
    findall(Y, (member(point(_,Y),Points);
                member(h(_,Y),Points);
                member(v(_,Y),Points)), Ys),
    max_list(Xs, X),
    max_list(Ys, Y).

draw(Points, along(Xy,V)) :-
    frame(Points, [Xm,Ym]),
    (x = Xy,
     !, findall(v(V,Y), between(0,Ym,Y), Line);
     y = Xy,
     !, findall(h(X,V), between(0,Xm,X), Line)
    ),
    append(Line, Points, Ps),
    draw(Ps).

draw(Points) :-
    frame(Points, [Xm,Ym]),
    foreach(between(0, Ym, Y),
            (foreach(between(0, Xm, X),
                    (member(point(X,Y), Points),
                     !, write('#');
                     member(h(X,Y), Points),
                     !, write('-');
                     member(v(X,Y), Points),
                     !, write('|');
                     write('.')
                    )),
            write('\n'))).

show_count(Points) :-
    length(Points, N),
    format("~I visible dots~n", [N]).

solution :-
    data(Input),
    Points-Commands = Input,
    format("points:~n"),
    foreach(member(Line, Points),
            format(" ~p", [Line])),
    write('\n'),
    format("commands:~n"),
    foreach(member(Line, Commands),
            format(" ~p",[Line])),
    write('\n'),
    fold(Commands, Points, Points_1),
    format("result:~n"),
    draw(Points_1),
    show_count(Points_1),
    true.
