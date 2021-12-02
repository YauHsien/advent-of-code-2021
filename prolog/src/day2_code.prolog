default_matter(hposition, 0).
default_matter(depth, 0).
default_matter(aim, 0).

default_move_profile(hpos/HPosition-dep/Depth) :-
    default_matter(hposition, HPosition),
    default_matter(dep, Depth).

default_aim_move_profile(hpos/HPosition-dep/Depth-aim/Aim) :-
    default_matter(hposition, HPosition),
    default_matter(depth, Depth),
    default_matter(aim, Aim).

collect_commands([], []).
collect_commands([row(LineAsAtom)|Rest], [(Command,Number)|Rest1]) :-
    atom(LineAsAtom),
    !,
    atom_codes(LineAsAtom, Line),
    split_string(Line, " ", "", [Command,NumberAsList]),
    atom_codes(NumberAsAtom, NumberAsList),
    atom_number(NumberAsAtom, Number),
    collect_commands(Rest, Rest1).

dive_matter(FilePath, ProductOfMatters) :-
    csv_read_file(FilePath, RawList),
    collect_commands(RawList, Commands),
    default_matter(hposition, HPosition),
    default_matter(depth, Depth),
    do_submarine_operation(Commands, HPosition, Depth, HPos1, Dep1),
    ProductOfMatters is HPos1 * Dep1.

do_submarine_operation([], HPos1, Dep1, HPosition, Depth) :-
    HPosition is HPos1,
    Depth is Dep1.
do_submarine_operation([("forward",N)|Rest], HPos1, Dep1, HPosition, Depth) :-
    do_submarine_operation(Rest, N+HPos1, Dep1, HPosition, Depth).
do_submarine_operation([("down",N)|Rest], HPos1, Dep1, HPosition, Depth) :-
    do_submarine_operation(Rest, HPos1, N+Dep1, HPosition, Depth).
do_submarine_operation([("up",N)|Rest], HPos1, Dep1, HPosition, Depth) :-
    do_submarine_operation(Rest, HPos1, -N+Dep1, HPosition, Depth).

%% -- Part 2 ---------------------------

do_submarine_operation([], hpos/HPosition-dep/Depth-aim/Aim, hpos/HPosition-dep/Depth-aim/Aim).
do_submarine_operation([("forward",N)|Rest], hpos/HPos-dep/Dep-aim/Aim1, hpos/HPosition-dep/Depth-aim/Aim) :-
    HPos1 is HPos + N,
    Dep1 is Dep + N * Aim1,
    do_submarine_operation(Rest, hpos/HPos1-dep/Dep1-aim/Aim1, hpos/HPosition-dep/Depth-aim/Aim).
do_submarine_operation([("down",N)|Rest], hpos/HPos1-dep/Dep1-aim/Aim1, hpos/HPosition-dep/Depth-aim/Aim) :-
    Aim2 is Aim1 + N,
    do_submarine_operation(Rest, hpos/HPos1-dep/Dep1-aim/Aim2, hpos/HPosition-dep/Depth-aim/Aim).
do_submarine_operation([("up",N)|Rest], hpos/HPos1-dep/Dep1-aim/Aim1, hpos/HPosition-dep/Depth-aim/Aim) :-
    Aim2 is Aim1 - N,
    do_submarine_operation(Rest,  hpos/HPos1-dep/Dep1-aim/Aim2, hpos/HPosition-dep/Depth-aim/Aim).

aim_dive_matter(FilePath, ProductOfMatters) :-
    csv_read_file(FilePath, RawList),
    collect_commands(RawList, Commands),
    default_aim_move_profile(Profile),
    do_submarine_operation(Commands, Profile, hpos/HPosition-dep/Depth-aim/_Aim),
    ProductOfMatters is HPosition * Depth.

%% -- Solution -----------------------
:- dive_matter('../inputs/day2_input.txt', 1868935).
:- aim_dive_matter('../inputs/day2_input.txt', 1965970888).
