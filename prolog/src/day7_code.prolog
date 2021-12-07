
%% -- Data -----------------------------------
file_path('../inputs/day7_input.txt').

data(Inputs) :-
    file_path(FilePath),
    csv_read_file(FilePath, [row(InputsAtom)], [separator(0'\s)]),
    input_atom_list(row(InputsAtom), Inputs).

input_atom_list(row(InputsAtom), Inputs) :-
    atom(InputsAtom),
    !,
    atom_chars(InputsAtom, CharList),
    input_atom_list(CharList, [[]], Inputs),
    !.

input_atom_list([], [List|Acc], Inputs) :-
    reverse_list_clock(List, Input),
    reverse([Input|Acc], Inputs).
input_atom_list([','|List], [Acc1|Acc], Inputs) :-
    reverse_list_clock(Acc1, Input),
    input_atom_list(List, [[],Input|Acc], Inputs).
input_atom_list([Char|List], [Acc1|Acc], Inputs) :-
    input_atom_list(List, [[Char|Acc1]|Acc], Inputs).

reverse_list_clock(List, Input) :-
    reverse(List, List1),
    atom_chars(Atom, List1),
    atom_number(Atom, Input).

%% --- Day 7: The Treachery of Whales ---
%% A giant whale has decided your submarine is its next meal, and it's much faster than you are. There's nowhere to run!

%% Suddenly, a swarm of crabs (each in its own tiny submarine - it's too deep for them otherwise) zooms in to rescue you! They seem to be preparing to blast a hole in the ocean floor; sensors indicate a massive underground cave system just beyond where they're aiming!

%% The crab submarines all need to be aligned before they'll have enough power to blast a large enough hole for your submarine to get through. However, it doesn't look like they'll be aligned before the whale catches you! Maybe you can help?

%% There's one major catch - crab submarines can only move horizontally.

%% You quickly make a list of the horizontal position of each crab (your puzzle input). Crab submarines have limited fuel, so you need to find a way to make all of their horizontal positions match while requiring them to spend as little fuel as possible.

%% For example, consider the following horizontal positions:

%% 16,1,2,0,4,2,7,1,2,14
%% This means there's a crab with horizontal position 16, a crab with horizontal position 1, and so on.

%% Each change of 1 step in horizontal position of a single crab costs 1 fuel. You could choose any horizontal position to align them all on, but the one that costs the least fuel is horizontal position 2:

%% Move from 16 to 2: 14 fuel
%% Move from 1 to 2: 1 fuel
%% Move from 2 to 2: 0 fuel
%% Move from 0 to 2: 2 fuel
%% Move from 4 to 2: 2 fuel
%% Move from 2 to 2: 0 fuel
%% Move from 7 to 2: 5 fuel
%% Move from 1 to 2: 1 fuel
%% Move from 2 to 2: 0 fuel
%% Move from 14 to 2: 12 fuel
%% This costs a total of 37 fuel. This is the cheapest possible outcome; more expensive outcomes include aligning at position 1 (41 fuel), position 3 (39 fuel), or position 10 (71 fuel).

%% Determine the horizontal position that the crabs can align to using the least fuel possible. How much fuel must they spend to align to that position?

alignment_cost(Inputs, First_pos, Last_pos, Fuel_list) :-
    normal_seq([First_pos,Last_pos], [Pos1,Pos2]),
    findall(N, between(Pos1,Pos2,N), Positions),
    findall((S,P)-Fs, (member(P,Positions),inputs_fuel_list(Inputs,P,Fs),sumlist(Fs,S)), Fuel_plan),
    list_to_assoc(Fuel_plan, Fuel_plan_assoc),
    %format("inspect: ~p~n", [Fuel_plan_assoc]),
    min_assoc(Fuel_plan_assoc, (Sum,Pos), Fuel_list),
    format("found: ~p ~p ~p~n", [Sum, Pos, Fuel_list]),
    true.

inputs_fuel_list(Inputs, Pos, Fuel_list) :-
    findall(N, (member(I,Inputs),diff(I,Pos,N)), Fuel_list).

normal_seq([N,M], [N,M]) :-
    N < M,
    !.
normal_seq([N,M], [M,N]).

diff(N, M, N1) :-
    N >= M,
    !,
    N1 is N - M.
diff(N, M, M1) :-
    M1 is M - N.

solution :-
    data(Inputs),
    max_list(Inputs, First_pos),
    min_list(Inputs, Last_pos),
    alignment_cost(Inputs, First_pos, Last_pos, Fuel_list),
    sum_list(Fuel_list, Sum),
    format("fuel: ~I~n", [Sum]),
    true.

%% --- Part Two ---
%% The crabs don't seem interested in your proposed solution. Perhaps you misunderstand crab engineering?

%% As it turns out, crab submarine engines don't burn fuel at a constant rate. Instead, each change of 1 step in horizontal position costs 1 more unit of fuel than the last: the first step costs 1, the second step costs 2, the third step costs 3, and so on.

%% As each crab moves, moving further becomes more expensive. This changes the best horizontal position to align them all on; in the example above, this becomes 5:

%% Move from 16 to 5: 66 fuel
%% Move from 1 to 5: 10 fuel
%% Move from 2 to 5: 6 fuel
%% Move from 0 to 5: 15 fuel
%% Move from 4 to 5: 1 fuel
%% Move from 2 to 5: 6 fuel
%% Move from 7 to 5: 3 fuel
%% Move from 1 to 5: 10 fuel
%% Move from 2 to 5: 6 fuel
%% Move from 14 to 5: 45 fuel
%% This costs a total of 168 fuel. This is the new cheapest possible outcome; the old alignment position (2) now costs 206 fuel instead.

%% Determine the horizontal position that the crabs can align to using the least fuel possible so they can make you an escape route! How much fuel must they spend to align to that position?

another_alignment_cost(Inputs, First_pos, Last_pos, DiffPred, Fuel_list) :-
    normal_seq([First_pos,Last_pos], [Pos1,Pos2]),
    findall(N, between(Pos1,Pos2,N), Positions),
    findall((S,P)-Fs, (member(P,Positions),inputs_fuel_list(Inputs,P,DiffPred,Fs),sumlist(Fs,S)), Fuel_plan),
    list_to_assoc(Fuel_plan, Fuel_plan_assoc),
    %format("inspect: ~p~n", [Fuel_plan_assoc]),
    min_assoc(Fuel_plan_assoc, (Sum,Pos), Fuel_list),
    format("found: ~p ~p ~p~n", [Sum, Pos, Fuel_list]),
    true.

inputs_fuel_list(Inputs, Pos, DiffPred, Fuel_list) :-
    findall(N, (member(I,Inputs),apply(DiffPred,[I,Pos,N])), Fuel_list).

factorial_diff(N, M, N1) :-
    N >= M,
    !,
    N2 is N - M,
    factorial(N2, N1).
factorial_diff(N, M, M1) :-
    M2 is M - N,
    factorial(M2, M1).

factorial(N, M) :-
    findall(E,between(1,N,E),List),
    sumlist(List, M).

solution_2 :-
    data(Inputs),
    max_list(Inputs, First_pos),
    min_list(Inputs, Last_pos),
    another_alignment_cost(Inputs, First_pos, Last_pos, factorial_diff, Fuel_list),
    sum_list(Fuel_list, Sum),
    format("fuel: ~I~n", [Sum]),
    true.
