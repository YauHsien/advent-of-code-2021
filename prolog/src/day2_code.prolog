default_matter(hposition, 0).
default_matter(depth, 0).

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

%% -- Solution -----------------------
:- dive_matter('../inputs/day2_input.txt', 1868935).
