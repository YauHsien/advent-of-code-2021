code(0, C) :-
    atom_codes('0', [C]).
code(1, C) :-
    atom_codes('1', [C]).

base_acc(N, List) :-
    bagof(Acc, limit(N,(Acc=1:0/0:0,repeat)), List).

collect_input([], []).
collect_input([row(Input)|Rest], [Codes|Rest1]) :-
    atom(Input),
    !,
    atom_codes(Input, Codes),
    collect_input(Rest, Rest1).

data_len([], 0).
data_len([E], N) :-
    !,
    length(E, N).
data_len([E|T], N) :-
    data_len(T, N1),
    length(E, N2),
    (N1 >= N2, !, N is N1;
     N is N2).

report_power_consumption(FilePath, PowerConsumption) :-
    csv_read_file(FilePath, RawList, [convert(false)]),
    collect_input(RawList, InputList),
    data_len(InputList, DataWidth),
    base_acc(DataWidth, Acc),
    zip*(InputList, Acc, Acc1),
    acc_to_gamma_and_epsilon_rates(Acc1, GammaRate1, EpsilonRate1),
    binary_codes_integer(GammaRate1, G),
    binary_codes_integer(EpsilonRate1, E),
    PowerConsumption is (G) * (E).

zip*([], Acc, Acc).
zip*([Term|Rest], Acc, Result) :-
    zip(Term, Acc, Acc1),
    zip*(Rest, Acc1, Result).

zip([], [], []).
zip([C|Rest], [1:C1/0:C0|Rest1], [1:C1/0:(C0+1)|Rest2]) :-
    code(0, C),
    !,
    zip(Rest, Rest1, Rest2).
zip([C|Rest], [1:C1/0:C0|Rest1], [1:(C1+1)/0:C0|Rest2]) :-
    code(1, C),
    !,
    zip(Rest, Rest1, Rest2).

acc_to_gamma_and_epsilon_rates([], [], []).
acc_to_gamma_and_epsilon_rates([1:C1/0:C0|Acc], [G|GammaRate], [E|EpsilonRate]) :-
    C1_ is C1,
    C0_ is C0,
    (C1_ >= C0_,
     !,
     code(1, G),
     code(0, E);
     code(0, G),
     code(1, E)
    ),
    acc_to_gamma_and_epsilon_rates(Acc, GammaRate, EpsilonRate).

binary_codes_integer(Codes, DecimalExp) :-
    binary_codes_integer(Codes, 0, DecimalExp).

binary_codes_integer([49], Acc, Acc+1).
binary_codes_integer([48], Acc, Acc) :-
    !.
binary_codes_integer([49|Rest], Acc, DecimalExp) :-
    !,
    length(Rest, L),
    binary_codes_integer(Rest, Acc+(2**L), DecimalExp).
binary_codes_integer([48|Rest], Acc, DecimalExp) :-
    binary_codes_integer(Rest, Acc, DecimalExp).

%% -- Part2 ---------------------------------------------------------

collect_input_with_indices([], []).
collect_input_with_indices([row(Input)|Rest], [Index/Input|Rest1]) :-
    atom(Input),
    !,
    atom_chars(Input, Index),
    collect_input_with_indices(Rest, Rest1).

report_life_support_rating(FilePath, LifeSupportRating) :-
    csv_read_file(FilePath, RawList, [convert(false)]),
    collect_input_with_indices(RawList, InputList),
    partition(InputList, [], [], Group1i, Group2i),
    determine(Group1i, Group2i, most_common, G1i, G2i),
    go_with(G1i, most_common, [OxygenGeneratorRating]),
    go_with(G2i, least_common, [CO2ScrubberRating]),
    atom_decimal(OxygenGeneratorRating, O),
    atom_decimal(CO2ScrubberRating, C),
    LifeSupportRating is (O) * (C).

partition([], Group1, Group2, Group1, Group2).
partition([Item|List], Group1, Group2, Group1f, Group2f) :-
    classify(Item, Group1, Group2, Group1a, Group2a),
    partition(List, Group1a, Group2a, Group1f, Group2f).

classify(['1'|Index]/Value, Group1, Group2, [Index/Value|Group1], Group2).
classify(['0'|Index]/Value, Group1, Group2, Group1, [Index/Value|Group2]).

determine(Group1, Group2, most_common, G1, G2) :-
    length(Group1, L1),
    length(Group2, L2),
    (L1 >= L2,
     !,
     G1 = Group1,
     G2 = Group2;
     G1 = Group2,
     G2 = Group1
    ).
determine(Group1, Group2, least_common, G1, G2) :-
    length(Group1, L1),
    length(Group2, L2),
    (L1 < L2,
     !,
     G1 = Group1,
     G2 = Group2;
     G1 = Group2,
     G2 = Group1
    ).

go_with1(Group, CommonBy, GroupR) :-
    partition(Group, [], [], G1, G2),
    determine(G1, G2, CommonBy, Group1, _Group2),
    GroupR = Group1.

go_with([[]/Found], _, [Found]) :-
    !.
go_with([_/Found], _, [Found]) :-
    !.
go_with(Group, CommonBy, GroupR) :-
    partition(Group, [], [], G1, G2),
    determine(G1, G2, CommonBy, Group1, _Group2),
    go_with(Group1, CommonBy, GroupR).

atom_decimal(Atom, Dec) :-
    atom_chars(Atom, Chars),
    chars_decimal(Chars, Dec).

chars_decimal(Codes, DecimalExp) :-
    chars_decimal(Codes, 0, DecimalExp).

chars_decimal(['1'], Acc, Acc+1).
chars_decimal(['0'], Acc, Acc) :-
    !.
chars_decimal(['1'|Rest], Acc, DecimalExp) :-
    !,
    length(Rest, L),
    chars_decimal(Rest, Acc+(2**L), DecimalExp).
chars_decimal(['0'|Rest], Acc, DecimalExp) :-
    chars_decimal(Rest, Acc, DecimalExp).

%% -- Solutions -------------------------------------------------
:- report_power_consumption('../inputs/day3_input.txt', 3958484).
:- report_life_support_rating('../inputs/day3_input.txt', 1613181).
