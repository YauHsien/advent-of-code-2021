:- module(day11_code, []). % Module definition in SWI-Prolog

_Module.add(b,_) := b :- !.
_Module.add(s,_) := s :- !.
_Module.add(_,b) := b :- !.
_Module.add(e,_) := e :- !.
_Module.add(_,e) := e :- !.
_Module.add(X,Y) := e :-
                 X + Y > 9,
                 !.
_Module.add(X,Y) := X+Y.

_Module.normalize(b) := '_' :- !.
_Module.normalize(x) := 'x' :- !.
_Module.normalize(s) := 's' :- !.
_Module.normalize(e) := '*' :- !.
_Module.normalize(N) := Z :-
            %(N > 6; N < 1),
            !,
            Z is N.
_Module.normalize(_) := ' '.

%% -- Data -----------------------------------
file_path('../inputs/day11_input.txt').

data(Inputs) :-
    file_path(FilePath),
    csv_read_file(FilePath, Rows, [convert(false)]),
    input_atom_list(Rows, Inputs).

input_atom_list([], []).
input_atom_list([row(Atom)|Rows], [List|Inputs]) :-
    atom_chars(Atom, AtomList),
    findall(N, (member(E,AtomList),atom_number(E,N)), List),
    input_atom_list(Rows, Inputs).

%% --- Day 11: Dumbo Octopus ---
%% You enter a large cavern full of rare bioluminescent dumbo octopuses! They seem to not like the Christmas lights on your submarine, so you turn them off for now.
%
%% There are 100 octopuses arranged neatly in a 10 by 10 grid. Each octopus slowly gains energy over time and flashes brightly for a moment when its energy is full. Although your lights are off, maybe you could navigate through the cave without disturbing the octopuses if you could predict when the flashes of light will happen.
%
%% Each octopus has an energy level - your submarine can remotely measure the energy level of each octopus (your puzzle input). For example:
%
%% 5483143223
%% 2745854711
%% 5264556173
%% 6141336146
%% 6357385478
%% 4167524645
%% 2176841721
%% 6882881134
%% 4846848554
%% 5283751526
%% The energy level of each octopus is a value between 0 and 9. Here, the top-left octopus has an energy level of 5, the bottom-right one has an energy level of 6, and so on.
%
%% You can model the energy levels and flashes of light in steps. During a single step, the following occurs:
%
%% First, the energy level of each octopus increases by 1.
%% Then, any octopus with an energy level greater than 9 flashes. This increases the energy level of all adjacent octopuses by 1, including octopuses that are diagonally adjacent. If this causes an octopus to have an energy level greater than 9, it also flashes. This process continues as long as new octopuses keep having their energy level increased beyond 9. (An octopus can only flash at most once per step.)
%% Finally, any octopus that flashed during this step has its energy level set to 0, as it used all of its energy to flash.
%% Adjacent flashes can cause an octopus to flash on a step even if it begins that step with very little energy. Consider the middle octopus with 1 energy in this situation:
%
%% Before any steps:
%% 11111
%% 19991
%% 19191
%% 19991
%% 11111
%
%% After step 1:
%% 34543
%% 40004
%% 50005
%% 40004
%% 34543
%
%% After step 2:
%% 45654
%% 51115
%% 61116
%% 51115
%% 45654
%% An octopus is highlighted when it flashed during the given step.
%
%% Here is how the larger example above progresses:
%
%% Before any steps:
%% 5483143223
%% 2745854711
%% 5264556173
%% 6141336146
%% 6357385478
%% 4167524645
%% 2176841721
%% 6882881134
%% 4846848554
%% 5283751526
%
%% After step 1:
%% 6594254334
%% 3856965822
%% 6375667284
%% 7252447257
%% 7468496589
%% 5278635756
%% 3287952832
%% 7993992245
%% 5957959665
%% 6394862637
%
%% After step 2:
%% 8807476555
%% 5089087054
%% 8597889608
%% 8485769600
%% 8700908800
%% 6600088989
%% 6800005943
%% 0000007456
%% 9000000876
%% 8700006848
%
%% After step 3:
%% 0050900866
%% 8500800575
%% 9900000039
%% 9700000041
%% 9935080063
%% 7712300000
%% 7911250009
%% 2211130000
%% 0421125000
%% 0021119000
%
%% After step 4:
%% 2263031977
%% 0923031697
%% 0032221150
%% 0041111163
%% 0076191174
%% 0053411122
%% 0042361120
%% 5532241122
%% 1532247211
%% 1132230211
%
%% After step 5:
%% 4484144000
%% 2044144000
%% 2253333493
%% 1152333274
%% 1187303285
%% 1164633233
%% 1153472231
%% 6643352233
%% 2643358322
%% 2243341322
%
%% After step 6:
%% 5595255111
%% 3155255222
%% 3364444605
%% 2263444496
%% 2298414396
%% 2275744344
%% 2264583342
%% 7754463344
%% 3754469433
%% 3354452433
%
%% After step 7:
%% 6707366222
%% 4377366333
%% 4475555827
%% 3496655709
%% 3500625609
%% 3509955566
%% 3486694453
%% 8865585555
%% 4865580644
%% 4465574644
%
%% After step 8:
%% 7818477333
%% 5488477444
%% 5697666949
%% 4608766830
%% 4734946730
%% 4740097688
%% 6900007564
%% 0000009666
%% 8000004755
%% 6800007755
%
%% After step 9:
%% 9060000644
%% 7800000976
%% 6900000080
%% 5840000082
%% 5858000093
%% 6962400000
%% 8021250009
%% 2221130009
%% 9111128097
%% 7911119976
%
%% After step 10:
%% 0481112976
%% 0031112009
%% 0041112504
%% 0081111406
%% 0099111306
%% 0093511233
%% 0442361130
%% 5532252350
%% 0532250600
%% 0032240000
%% After step 10, there have been a total of 204 flashes. Fast forwarding, here is the same configuration every 10 steps:
%
%% After step 20:
%% 3936556452
%% 5686556806
%% 4496555690
%% 4448655580
%% 4456865570
%% 5680086577
%% 7000009896
%% 0000000344
%% 6000000364
%% 4600009543
%
%% After step 30:
%% 0643334118
%% 4253334611
%% 3374333458
%% 2225333337
%% 2229333338
%% 2276733333
%% 2754574565
%% 5544458511
%% 9444447111
%% 7944446119
%
%% After step 40:
%% 6211111981
%% 0421111119
%% 0042111115
%% 0003111115
%% 0003111116
%% 0065611111
%% 0532351111
%% 3322234597
%% 2222222976
%% 2222222762
%
%% After step 50:
%% 9655556447
%% 4865556805
%% 4486555690
%% 4458655580
%% 4574865570
%% 5700086566
%% 6000009887
%% 8000000533
%% 6800000633
%% 5680000538
%
%% After step 60:
%% 2533334200
%% 2743334640
%% 2264333458
%% 2225333337
%% 2225333338
%% 2287833333
%% 3854573455
%% 1854458611
%% 1175447111
%% 1115446111
%
%% After step 70:
%% 8211111164
%% 0421111166
%% 0042111114
%% 0004211115
%% 0000211116
%% 0065611111
%% 0532351111
%% 7322235117
%% 5722223475
%% 4572222754
%
%% After step 80:
%% 1755555697
%% 5965555609
%% 4486555680
%% 4458655580
%% 4570865570
%% 5700086566
%% 7000008666
%% 0000000990
%% 0000000800
%% 0000000000
%
%% After step 90:
%% 7433333522
%% 2643333522
%% 2264333458
%% 2226433337
%% 2222433338
%% 2287833333
%% 2854573333
%% 4854458333
%% 3387779333
%% 3333333333
%
%% After step 100:
%% 0397666866
%% 0749766918
%% 0053976933
%% 0004297822
%% 0004229892
%% 0053222877
%% 0532222966
%% 9322228966
%% 7922286866
%% 6789998766
%% After 100 steps, there have been a total of 1656 flashes.
%
%% Given the starting energy levels of the dumbo octopuses in your cavern, simulate 100 steps. How many total flashes are there after 100 steps?

prepare(Lists, Result) :-
    [H|_] = Lists,
    length(H, N),
    N1 is N + 2,
    findall(b, between(1,N1,_), List_1),
    findall(List_2, (member(List,Lists),append([b|List],[b],List_2)), Result_1),
    append([List_1|Result_1], [List_1], Result).

find_flash_times(0, _, 0) :-
    !.
find_flash_times(N, Lists, Count_1+Count_2) :-
    N1 is N - 1,
    step(Lists, Result, Count_1),
    find_flash_times(N1, Result, Count_2).


step(Lists, Result, Flashes) :-
    groth(Lists, Result_1),
    collect_energy_until_not_again(Result_1, Result, Flashes).
%step(Lists,_) :-
%    format("FFOMO: ~p~n", [Lists]),fail.

%collect_energy(Lists, Lists, _Again) :-
%    format("FOMO: ~p~n", [Lists]),
%    true,!.

collect_energy_until_not_again(Lists, Result, Flashes) :-
    collect_energy(Lists, Result_1, Again),
    %format("---~n"),
    %show(Result_1),
    (Again,
     !,
     reflow(Result_1, Result_2),
     %format("xxx"),
     %show(Result_2),
     collect_energy_until_not_again(Result_2, Result, Flashes);
     %show(Result_1),
     count_flashes(Result_1, Flashes),
     end_flashes(Result_1, Result)
    ).

collect_energy([[N1, N2, N3 | Rest1],
                [N4,  C, N5 | Rest2],
                [N6, N7, N8 | Rest3] |
                Rests
               ],
               [[N1,  N2,  N3 | Rest1],
                [N4,  Cn,  Nn5 | Result2],
                [N6, Nn7, Nn8 | Result3] |
                Results
               ],
           Again) :-
    !,
    ((e = C;
      s = C),
     !,
     Cn = C;
     findall(N, (member(N,[N1,N2,N3,N4,N5,N6,N7,N8]),N==e), FlashingList),
     %format("C: ~p ~p --- ~p~n", [C, [N1,N2,N3,N4,N5,N6,N7,N8], FlashingList]),
     length(FlashingList, N),
     (N =\= 0,
      !,
      Module = day11_code{},
      (Cn_2 = Module.add(C, N),
                     (e = Cn_2,
                      !,
                      Again_0 = true,
                      Cn = x;
                      Cn = Cn_2
                     )
      );
      Cn = C
     )
    ),
    %% In the begining, newly activating cell is marked as 'x'.
    %% e: the cell is activating and flashs.
    %format("Cn: ~p~n", [Cn]),
    %format("emit horizontal: ~p~n", [[[N2, N3 | Rest1],
    %                                  [Cn, N5 | Rest2],
    %                                  [N7, N8 | Rest3]
    %                                 ]]),
    collect_energy([[N2, N3 | Rest1],
                    [Cn, N5 | Rest2],
                    [N7, N8 | Rest3]
                   ],
                   [[N2, N3  | Rest1],
                    [Cn, Nn5 | Result2],
                    [N7, N8  | Rest3]
                   ],
                   Again_1),
    %format("~p~n", [[[N2, N3 | Rest1],
    %                 [Cn, Nn5 | Result2],
    %                 [N7, N8 | Rest3]
    %                ]]),
    %format("emit virtital:~n~p~n", [[[N4, Cn, Nn5 | Result2],
    %                                [N6, N7, N8  | Rest3] |
    %                                Rests
    %                               ]]),
    collect_energy([[N4, Cn, Nn5 | Result2],
                    [N6, N7, N8  | Rest3] |
                    Rests
                   ],
                   [[N4,  Cn, Nn5 | Result2],
                    [N6, Nn7, Nn8 | Result3] |
                    Results
                   ],
                   Again_2),
    %format("~p~n~p~n", [%[[[N4, Cn,  Nn5 | Result2],
    %                  %[Nn6, Nn7, Nn8 | Result3] |
    %                  %Results
    %                 %]
    %                  %          ],
    %                  %          Again,
    %           Again,
    %           [
    %               [N4, Cn,  Nn5 | Result2],
    %               [N6, Nn7, Nn8 | Result3] |
    %               Results
    %           ]]).
    (not(var(Again_0)), Again_0,
     !,
     Again = Again_0;
     not(var(Again_1)), Again_1,
     !,
     Again = Again_1;
     not(var(Again_2)), Again_2,
     !,
     Again = Again_2;
     Again = false
    ),
    true.
collect_energy(Lists, Lists, _Again) :-
    %format("FOMO: ~p~n", [Lists]),
    true.

reflow(Lists, Results) :-
    findall(List_1,
            (member(List_0, Lists),
             findall(N,
                     (member(E, List_0),
                      (e = E,
                       N = s;
                       x = E,
                       N = e;
                       e \= E,
                       x \= E,
                       N = E
                      )
                     ),
                     List_1)
            ),
            Results).

count_flashes(Lists, Count) :-
    findall(N,
            (member(List_0, Lists),
             findall(N,
                     (member(E, List_0),
                      (s = E;
                       e = E
                      )
                     ),
                     List_1),
             length(List_1, N)
            ),
            Lists_1),
    sumlist(Lists_1, Count).

end_flashes(Lists, Results) :-
    findall(List_1,
            (member(List_0, Lists),
             findall(N,
                     (member(E, List_0),
                      (s = E,
                       N = 0;
                       e = E,
                       N = 0;
                       e \= E,
                       s \= E,
                       N = E
                      )
                     ),
                     List_1)
            ),
            Results).

groth(Lists, Result) :-
    Module = day11_code{},
    findall(List_1,
            (member(List, Lists),
             findall(X,
                     (member(E, List),
                      X = Module.add(E, 1)
                     ),
                     List_1)
            ),
            Result).

show(Lists) :-
    Module = day11_code{},
    foreach(member(List, Lists),
            (foreach((member(E,List),E_1 = Module.normalize(E)),
                     format("~w",[E_1])),
             format("~n"))).

solution :-
    data(Input),
    prepare(Input, Input_1),
    %step(Input_1, Result, C0),
    %step(Result, Result_1, C1),
    %step(Result_1, Result_2, C2),
    %step(Result_2, Result_3, C3),
    %step(Result_3, Result_4, C4),
    %step(Result_4, Result_5, C5),
    %step(Result_5, Result_6, C6),
    %step(Result_6, Result_7, C7),
    %step(Result_7, Result_8, C8),
    %step(Result_8, Result_9, C9),

    %show(Result),
    %show(Result_1),
    %show(Result_2),
    %show(Result_3),
    %show(Result_4),
    %show(Result_5),
    %show(Result_6),
    %show(Result_7),
    %show(Result_8),
    %show(Result_9),

    %sumlist([C0,C1,C2,C3,C4,C5,C6,C7,C8,C9], Sum),
    %format("flashes: ~p~n", [Sum]),

    find_flash_times(100, Input_1, Count),
    format("~I = ~p~n", [Count, Count]),

    true.
