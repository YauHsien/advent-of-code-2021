%% --- Day 4: Giant Squid ---
%% You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't see any sunlight. What you can see, however, is a giant squid that has attached itself to the outside of your submarine.
%
%% Maybe it wants to play bingo?
%
%% Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random, and the chosen number is marked on all boards on which it appears. (Numbers may not appear on all boards.) If all numbers in any row or any column of a board are marked, that board wins. (Diagonals don't count.)
%
%% The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the time. It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input). For example:
%
%% 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
%
%% 22 13 17 11  0
%%  8  2 23  4 24
%% 21  9 14 16  7
%%  6 10  3 18  5
%%  1 12 20 15 19
%
%%  3 15  0  2 22
%%  9 18 13 17  5
%% 19  8  7 25 23
%% 20 11 10 24  4
%% 14 21 16 12  6
%
%% 14 21 17 24  4
%% 10 16 15  9 19
%% 18  8 23 26 20
%% 22 11 13  6  5
%%  2  0 12  3  7
%% After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no winners, but the boards are marked as follows (shown here adjacent to each other to save space):
%
%% 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
%%  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
%% 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
%%  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
%%  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
%% After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are still no winners:
%
%% 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
%%  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
%% 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
%%  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
%%  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
%% Finally, 24 is drawn:
%
%% 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
%%  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
%% 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
%%  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
%%  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
%% At this point, the third board wins because it has at least one complete row or column of marked numbers (in this case, the entire top row is marked: 14 21 17 24 4).
%
%% The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board won, 24, to get the final score, 188 * 24 = 4512.
%
%% To guarantee victory against the giant squid, figure out which board will win first. What will your final score be if you choose that board?

row(_, _, _, _, _).

init_board_buffer([]).

configure([InputCommands,row('')|InputBoards], Commands, Boards) :-
    configure_boards(InputBoards, Boards),
    configure_commands(InputCommands, Commands).

configure_boards(InputBoards, Boards) :-
    init_board_buffer(New_board_buffer),
    configure_boards(InputBoards, New_board_buffer, Boards).

configure_boards([], Board_buffer, [New_board]) :-
    build_board(Board_buffer, New_board).
configure_boards([row('')|Rest], Board_buffer, [New_board|Boards]) :-
    !,
    build_board(Board_buffer, New_board),
    configure_boards(Rest, Boards).
configure_boards([Row|Rest], Board_buffer, Boards) :-
    Row,
    configure_boards(Rest, [Row|Board_buffer], Boards).

build_board(Buffer, Board) :-
    init_board_buffer(New_buffer),
    build_board(Buffer, New_buffer, Board).

build_board([], Board, Board).
build_board([Row|Rest], Buffer, Board) :-
    row(C1,C2,C3,C4,C5) = Row,
    build_board(Rest, [[C1,C2,C3,C4,C5]|Buffer], Board).

configure_commands(row(SepAtom), Commands) :-
    !,
    atom_chars(SepAtom, Chars),
    configure_commands(Chars, [[]], Commands).

configure_commands([], [Chars|Acc], Commands) :-
    reverse(Chars, Chars1),
    atom_chars(Atom, Chars1),
    atom_number(Atom, Number),
    reverse([Number|Acc], Commands).
configure_commands([','|Rest], [Chars|Acc], Commands) :-
    reverse(Chars, Chars1),
    atom_chars(Atom, Chars1),
    atom_number(Atom, Number),
    configure_commands(Rest, [[]|[Number|Acc]], Commands).
configure_commands([C|Rest], [Chars|Acc], Commands) :-
    C \= 0',,
    configure_commands(Rest, [[C|Chars]|Acc], Commands).

mark_boards(_N, [], []).
mark_boards(N, [Board|Boards], [Board1|Boards1]) :-
    mark_board(N, Board, Board1),
    mark_boards(N, Boards, Boards1).

mark_board(_N, [], []).
mark_board(N, [Line|Board], [New_line|New_board]) :-
    is_list(Line),
    !,
    mark_board(N, Line, New_line),
    mark_board(N, Board, New_board).
mark_board(N, [N|Line], [m(N)|New_line]) :-
    !,
    mark_board(N, Line, New_line).
mark_board(N, [M|Line], [M|New_line]) :-
    mark_board(N, Line, New_line).

add_up_non_marked_numbers([], 0).
add_up_non_marked_numbers([List|Board], N_list+N_board) :-
    is_list(List),
    !,
    add_up_non_marked_numbers(List, N_list),
    add_up_non_marked_numbers(Board, N_board).
add_up_non_marked_numbers([N|List], N+N_list) :-
    number(N),
    !,
    add_up_non_marked_numbers(List, N_list).
add_up_non_marked_numbers([_|List], N_list) :-
    add_up_non_marked_numbers(List, N_list).

bingo([[m(_), m(_), m(_), m(_), m(_)]|_Rest]) :- !.
bingo([_|List]) :- bingo(List), !.
bingo([[m(_)|_], [m(_)|_], [m(_)|_], [m(_)|_], [m(_)|_]]) :- !.
bingo([[_|R1], [_|R2], [_|R3], [_|R4], [_|R5]]) :- bingo([R1,R2,R3,R4,R5]).

play_bingo(_Boards, [], nil, []).
play_bingo(Boards, [Command|Commands], BingoNumber, BingoBoards) :-
    mark_boards(Command, Boards, Boards1),
    (bagof(Board, (member(Board,Boards1), bingo(Board)), BingoBoards),
     !,
     BingoNumber = Command;
     play_bingo(Boards1, Commands, BingoNumber, BingoBoards)
    ).

%% -- Solution -----------------------------------
file_path('../inputs/day4_input.txt').
:- file_path(FilePath),
   csv_read_file(FilePath, InputRows, [strip(true),separator(0'\s),match_arity(false)]),
   configure(InputRows, Commands, Boards),
   play_bingo(Boards, Commands, Just_called, [Board|Others]),
   format("Bingo #: ~p~n", [Just_called]),
   write('Bingo baord: '),
   write(Board),
   write('\n'),
   write('Other bingo board(s): '),
   write(Others),
   write('\n'),
   add_up_non_marked_numbers(Board, Sum),
   Final_score is Just_called * (Sum),
   format("Final score: ~p * ~p = ~p~n", [Just_called, (Sum), Final_score]),
   true.

%% --- Part Two ---
%% On the other hand, it might be wise to try a different strategy: let the giant squid win.
%
%% You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time counting its arms, the safe thing to do is to figure out which board will win last and choose that one. That way, no matter which boards it picks, it will win for sure.
%
%% In the above example, the second board is the last to win, which happens after 13 is eventually called and its middle column is completely marked. If you were to keep playing until this point, the second board would have a sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.
%
%% Figure out which board will win last. Once it wins, what would its final score be?

find_last_win(_Boards, [], _Win_set, Bingo_number, Last_win_board, Bingo_number, Last_win_board).
find_last_win(Boards, [Command|Commands], Ws, Bn, Lwb, Bingo_number, Last_win_board) :-
    mark_boards(Command, Boards, Boards1),
    (bagof(Board, (member(Board,Boards1), bingo(Board)), Ws1),
     !,
     length(Ws1, L_ws1),
     length(Ws, L_ws),
     (L_ws1 =:= L_ws,
      !,
      find_last_win(Boards1, Commands, Ws1, Bn, Lwb, Bingo_number, Last_win_board);
      mark_boards(Command, Ws, Ws2),
      bagof(Board, (member(Board,Ws1), not(member(Board,Ws2))), Lwb1),
      find_last_win(Boards1, Commands, Ws1, Command, Lwb1, Bingo_number, Last_win_board)
     );
     find_last_win(Boards1, Commands, Ws, Bn, Lwb, Bingo_number, Last_win_board)
    ).

show_scores([], _Just_called).
show_scores([Board|Boards], Just_called) :-
    add_up_non_marked_numbers(Board, Sum),
    Final_score is Just_called * (Sum),
    format("Final score: ~p * ~p = ~p~n", [Just_called, (Sum), Final_score]),
    show_scores(Boards, Just_called).

%% -- Solution -----------------------------------
:- file_path(FilePath),
   csv_read_file(FilePath, InputRows, [strip(true),separator(0'\s),match_arity(false)]),
   configure(InputRows, Commands, Boards),
   find_last_win(Boards, Commands, [], nil, nil, Just_called, [Board|Others]),
   format("Bingo #: ~p~n", [Just_called]),
   format("Last win board: ~p~n", [Board]),
   format("Other bingo board(s): ~p~n", [Others]),
   show_scores([Board|Others], Just_called),
   true.

