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

%report_life_support_rating(FilePath, InputList) :-
%    csv_read_file(FilePath, RawList, [convert(false)]),
%    collect_input(RawList, InputList).

%% -- Solutions -------------------------------------------------
:- report_power_consumption('../inputs/day3_input.txt', 3958484).
%:- report_life_support_rating('../inputs/day3_input.txt', 1613181).
