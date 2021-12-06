
%% -- Data -----------------------------------
file_path('../inputs/day6_input.txt').

data(Clocks) :-
    file_path(FilePath),
    csv_read_file(FilePath, [row(ClocksAtom)], [separator(0'\s)]),
    clocks_atom_list(row(ClocksAtom), Clocks).

clocks_atom_list(row(ClocksAtom), Clocks) :-
    atom(ClocksAtom),
    !,
    atom_chars(ClocksAtom, CharList),
    clocks_atom_list(CharList, [[]], Clocks).

clocks_atom_list([], [List|Acc], Clocks) :-
    reverse_list_clock(List, Clock),
    reverse([Clock|Acc], Clocks).
clocks_atom_list([','|List], [Acc1|Acc], Clocks) :-
    reverse_list_clock(Acc1, Clock),
    clocks_atom_list(List, [[],Clock|Acc], Clocks).
clocks_atom_list([Char|List], [Acc1|Acc], Clocks) :-
    clocks_atom_list(List, [[Char|Acc1]|Acc], Clocks).

reverse_list_clock(List, Clock) :-
    reverse(List, List1),
    atom_chars(Atom, List1),
    atom_number(Atom, Clock).

%% -- Part 1 ----------------------------------

%% --- Day 6: Lanternfish ---
%% The sea floor is getting steeper. Maybe the sleigh keys got carried this way?

%% A massive school of glowing lanternfish swims past. They must spawn quickly to reach such large numbers - maybe exponentially quickly? You should model their growth rate to be sure.

%% Although you know nothing about this specific species of lanternfish, you make some guesses about their attributes. Surely, each lanternfish creates a new lanternfish once every 7 days.

%% However, this process isn't necessarily synchronized between every lanternfish - one lanternfish might have 2 days left until it creates another lanternfish, while another might have 4. So, you can model each fish as a single number that represents the number of days until it creates a new lanternfish.

%% Furthermore, you reason, a new lanternfish would surely need slightly longer before it's capable of producing more lanternfish: two more days for its first cycle.

%% So, suppose you have a lanternfish with an internal timer value of 3:

%% After one day, its internal timer would become 2.
%% After another day, its internal timer would become 1.
%% After another day, its internal timer would become 0.
%% After another day, its internal timer would reset to 6, and it would create a new lanternfish with an internal timer of 8.
%% After another day, the first lanternfish would have an internal timer of 5, and the second lanternfish would have an internal timer of 7.
%% A lanternfish that creates a new fish resets its timer to 6, not 7 (because 0 is included as a valid timer value). The new lanternfish starts with an internal timer of 8 and does not start counting down until the next day.

%% Realizing what you're trying to do, the submarine automatically produces a list of the ages of several hundred nearby lanternfish (your puzzle input). For example, suppose you were given the following list:

%% 3,4,3,1,2
%% This list means that the first fish has an internal timer of 3, the second fish has an internal timer of 4, and so on until the fifth fish, which has an internal timer of 2. Simulating these fish over several days would proceed as follows:

%% Initial state: 3,4,3,1,2
%% After  1 day:  2,3,2,0,1
%% After  2 days: 1,2,1,6,0,8
%% After  3 days: 0,1,0,5,6,7,8
%% After  4 days: 6,0,6,4,5,6,7,8,8
%% After  5 days: 5,6,5,3,4,5,6,7,7,8
%% After  6 days: 4,5,4,2,3,4,5,6,6,7
%% After  7 days: 3,4,3,1,2,3,4,5,5,6
%% After  8 days: 2,3,2,0,1,2,3,4,4,5
%% After  9 days: 1,2,1,6,0,1,2,3,3,4,8
%% After 10 days: 0,1,0,5,6,0,1,2,2,3,7,8
%% After 11 days: 6,0,6,4,5,6,0,1,1,2,6,7,8,8,8
%% After 12 days: 5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8
%% After 13 days: 4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8
%% After 14 days: 3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8
%% After 15 days: 2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7
%% After 16 days: 1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8
%% After 17 days: 0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8
%% After 18 days: 6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8
%% Each day, a 0 becomes a 6 and adds a new 8 to the end of the list, while each other number decreases by 1 if it was present at the start of the day.

%% In this example, after 18 days, there are a total of 26 fish. After 80 days, there would be a total of 5934.

%% Find a way to simulate lanternfish. How many lanternfish would there be after 80 days?

down_to_birth(Clocks, Days, New_clocks) :-
    min_list(Clocks, Min_clock),
    birth(Min_clock, Clocks, New_borns),
    findall(N, (member(E,Clocks),next_clock(E,Min_clock,N)), New_clocks_1),
    append(New_clocks_1, New_borns, New_clocks),
    Days is Min_clock + 1.

next_clock(Clock, Min_clock, N) :-
    Clock =\= Min_clock,
    N is Clock - Min_clock - 1.
next_clock(Clock, Clock, 6).

birth(Min_clock, Clocks, New_borns) :-
    clumped(Clocks, Clumped),
    bagof(E, (member(Min_clock-E,Clumped)), List),
    sum_list(List, Birth_count),
    findall(8, between(1,Birth_count,_), New_borns).

until_day(Day_nth, Days_acc, Clocks_acc, Clocks) :-
    down_to_birth(Clocks_acc, Days, Clocks_1),
    Days_acc_1 is Days_acc + Days,
    length(Clocks_1, N),
    clumped(Clocks_1, G),
    format("day #~I: ~I ~p~n", [Days_acc_1, N, G]),
    (Days_acc_1 > Day_nth,
     !,
     Clocks = Clocks_acc;
     until_day(Day_nth, Days_acc_1, Clocks_1, Clocks)
    ).

%% -- Solution ---------------------------
:- data(Clocks),
   format("~p~n", [Clocks]),
   until_day(80, 0, Clocks, New),
   length(New, N),
   clumped(New, G),
   format("final: ~I ~p~n", [N,G]),
   true.

