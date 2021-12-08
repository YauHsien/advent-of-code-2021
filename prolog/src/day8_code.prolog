
%% -- Data -----------------------------------
file_path('../inputs/day8_input.txt').

data(Inputs) :-
    file_path(FilePath),
    csv_read_file(FilePath, Rows, [separator(0'\s)]),
    input_atom_list(Rows, Inputs).

input_atom_list([], []).
input_atom_list([row(I0,I1,I2,I3,I4,I5,I6,I7,I8,I9,'|',O0,O1,O2,O3)|Rows], [[I0,I1,I2,I3,I4,I5,I6,I7,I8,I9]-[O0,O1,O2,O3]|Inputs]) :-
    input_atom_list(Rows, Inputs).

%% --- Day 8: Seven Segment Search ---
%% You barely reach the safety of the cave when the whale smashes into the cave mouth, collapsing it. Sensors indicate another exit to this cave at a much greater depth, so you have no choice but to press on.

%% As your submarine slowly makes its way through the cave system, you notice that the four-digit seven-segment displays in your submarine are malfunctioning; they must have been damaged during the escape. You'll be in a lot of trouble without them, so you'd better figure out what's wrong.

%% Each digit of a seven-segment display is rendered by turning on or off any of seven segments named a through g:

%%   0:      1:      2:      3:      4:
%%  aaaa    ....    aaaa    aaaa    ....
%% b    c  .    c  .    c  .    c  b    c
%% b    c  .    c  .    c  .    c  b    c
%%  ....    ....    dddd    dddd    dddd
%% e    f  .    f  e    .  .    f  .    f
%% e    f  .    f  e    .  .    f  .    f
%%  gggg    ....    gggg    gggg    ....

%%   5:      6:      7:      8:      9:
%%  aaaa    aaaa    aaaa    aaaa    aaaa
%% b    .  b    .  .    c  b    c  b    c
%% b    .  b    .  .    c  b    c  b    c
%%  dddd    dddd    ....    dddd    dddd
%% .    f  e    f  .    f  e    f  .    f
%% .    f  e    f  .    f  e    f  .    f
%%  gggg    gggg    ....    gggg    gggg
%% So, to render a 1, only segments c and f would be turned on; the rest would be off. To render a 7, only segments a, c, and f would be turned on.

%% The problem is that the signals which control the segments have been mixed up on each display. The submarine is still trying to display numbers by producing output on signal wires a through g, but those wires are connected to segments randomly. Worse, the wire/segment connections are mixed up separately for each four-digit display! (All of the digits within a display use the same connections, though.)

%% So, you might know that only signal wires b and g are turned on, but that doesn't mean segments b and g are turned on: the only digit that uses two segments is 1, so it must mean segments c and f are meant to be on. With just that information, you still can't tell which wire (b/g) goes to which segment (c/f). For that, you'll need to collect more information.

%% For each display, you watch the changing signals for a while, make a note of all ten unique signal patterns you see, and then write down a single four digit output value (your puzzle input). Using the signal patterns, you should be able to work out which pattern corresponds to which digit.

%% For example, here is what you might see in a single entry in your notes:

%% acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
%% cdfeb fcadb cdfeb cdbaf
%% (The entry is wrapped here to two lines so it fits; in your notes, it will all be on a single line.)

%% Each entry consists of ten unique signal patterns, a | delimiter, and finally the four digit output value. Within an entry, the same wire/segment connections are used (but you don't know what the connections actually are). The unique signal patterns correspond to the ten different ways the submarine tries to render a digit using the current wire/segment connections. Because 7 is the only digit that uses three segments, dab in the above example means that to render a 7, signal lines d, a, and b are on. Because 4 is the only digit that uses four segments, eafb means that to render a 4, signal lines e, a, f, and b are on.

%% Using this information, you should be able to work out which combination of signal wires corresponds to each of the ten digits. Then, you can decode the four digit output value. Unfortunately, in the above example, all of the digits in the output value (cdfeb fcadb cdfeb cdbaf) use five segments and are more difficult to deduce.

%% For now, focus on the easy digits. Consider this larger example:

%% be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
%% fdgacbe cefdb cefbgd gcbe
%% edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
%% fcgedb cgb dgebacf gc
%% fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
%% cg cg fdcagb cbg
%% fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
%% efabcd cedba gadfec cb
%% aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
%% gecf egdcabf bgf bfgea
%% fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
%% gebdcfa ecba ca fadegcb
%% dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
%% cefg dcbef fcge gbcadfe
%% bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
%% ed bcgafe cdgba cbgef
%% egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
%% gbdfcae bgc cg cgb
%% gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
%% fgae cfgab fg bagce
%% Because the digits 1, 4, 7, and 8 each use a unique number of segments, you should be able to tell which combinations of signals correspond to those digits. Counting only digits in the output values (the part after | on each line), in the above example, there are 26 instances of digits that use a unique number of segments (highlighted above).

%% In the output values, how many times do digits 1, 4, 7, or 8 appear?

segments_count_display(2, 1).
segments_count_display(3, 7).
segments_count_display(4, 4).
segments_count_display(5, 2).
segments_count_display(5, 3).
segments_count_display(5, 5).
segments_count_display(6, 0).
segments_count_display(6, 6).
segments_count_display(6, 9).
segments_count_display(7, 8).

displays_segments_list(Displays, Data, Result) :-
    findall(Segments-Display, (member(Display,Displays),segments_count_display(Segments,Display)), Segment_count_list),
    findall(Common, (member(Input-Output,Data),displays_segments_list(Segment_count_list,Input,Output,Common)), Result_list),
    %format("r: ~p~n", [Result_list]),
    flatten(Result_list, List1),
    length(List1, Result).

displays_segments_list(Segments, Input, Output, Common) :-
    findall(Atom1-D, (member(Atom,Input),atom_chars(Atom,List),sort_atom(Atom,Atom1),length(List,N),member(N-D,Segments)), Input1),
    findall(Atom1-D, (member(Atom,Output),atom_chars(Atom,List),sort_atom(Atom,Atom1),length(List,N),member(N-D,Segments)), Output1),
    %format("i: ~p ~p ~p~n", [Segments, Input, Output]),
    %format("o: ~p ~p~n", [Input1, Output1]),
    bagof(E, (member(E,Output1),member(E,Input1)), Common).

sort_atom(Atom, Sort) :-
    atom_chars(Atom, List),
    msort(List, List1),
    atom_chars(Sort, List1).

solution :-
    data(Input),
    displays_segments_list([1,4,7,8], Input, Result),
    format("count: ~p~n", [Result]),
    true.

%% --- Part Two ---
%% Through a little deduction, you should now be able to determine the remaining digits. Consider again the first example above:

%% acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
%% cdfeb fcadb cdfeb cdbaf
%% After some careful analysis, the mapping between signal wires and segments only make sense in the following configuration:

%%  dddd
%% e    a
%% e    a
%%  ffff
%% g    b
%% g    b
%%  cccc
%% So, the unique signal patterns would correspond to the following digits:

%% acedgfb: 8
%% cdfbe: 5
%% gcdfa: 2
%% fbcad: 3
%% dab: 7
%% cefabd: 9
%% cdfgeb: 6
%% eafb: 4
%% cagedb: 0
%% ab: 1
%% Then, the four digits of the output value can be decoded:

%% cdfeb: 5
%% fcadb: 3
%% cdfeb: 5
%% cdbaf: 3
%% Therefore, the output value for this entry is 5353.

%% Following this same process for each entry in the second, larger example above, the output value of each entry can be determined:

%% fdgacbe cefdb cefbgd gcbe: 8394
%% fcgedb cgb dgebacf gc: 9781
%% cg cg fdcagb cbg: 1197
%% efabcd cedba gadfec cb: 9361
%% gecf egdcabf bgf bfgea: 4873
%% gebdcfa ecba ca fadegcb: 8418
%% cefg dcbef fcge gbcadfe: 4548
%% ed bcgafe cdgba cbgef: 1625
%% gbdfcae bgc cg cgb: 8717
%% fgae cfgab fg bagce: 4315
%% Adding all of the output values in this larger example produces 61229.

%% For each entry, determine all of the wire/segment connections and decode the four-digit output values. What do you get if you add up all of the output values?

guess_list(Inputs, Result) :-
    findall(Answer, (member(Input-Output,Inputs),
                     guess(Input,Output,Answer),
                     format("~p~n",[Output-Answer])
                    ), Result_list1),
    sumlist(Result_list1, Result).

guess(Input, Output, Answer) :-
    common_guess(Input, Guess),
    determine(Guess, SSD),
    prepare_SSD(SSD, Mapping),
    findall(A, (member(Atom,Output),render_by(Mapping,Atom,N),atom_number(A,N)), Ns),
    atom_chars(Atom, Ns),
    atom_number(Atom, Answer).

common_guess(Input, Guess) :-
    findall(I-Guess, (member(I, Input),
                      segment_guess(I, Guess)
                     ), Guess_list),
    findall(I-Gs, (member(I,Input),findall(G,member(I-G,Guess_list),Gs)), Guess_list_1),
    combinations(Guess_list_1, Guess).

segment_guess(Atom, Guess) :-
    atom_chars(Atom, List),
    length(List, N),
    segments_count_display(N, Guess).

combinations([], []).
combinations([H-List|T], [H-I|T1]) :-
    member(I, List),
    combinations(T, T1).

determine([List|Lists], SSD) :-
    is_list(List),
    !,
    flatten([List|Lists], List_1),
    determine(List_1, SSD).
determine(List, SSD) :-
    seven_segments_display(SSD),
    foreach(member(A-D, List),
            can_render(D, A, SSD)),
    determined_SSD(SSD).

seven_segments_display(
    (A, B, C, E, F, G,    % For '0' rendering
     C, F,                % For '1' rendering
     A, C, D, E, G,       % For '2' rendering
     A, C, D, F, G,       % For '3' rendering
     B, C, D, F,          % For '4' rendering
     A, B, D, F, G,       % For '5' rendering
     A, B, D, E, F, G,    % For '6' rendering
     A, C, F,             % For '7' rendering
     A, B, C, D, E, F, G, % For '8' rendering
     A, B, C, D, F, G     % For '9' rendering
    )).

can_render(Display, Atom, SSD) :-
    atom(Atom),
    !,
    atom_chars(Atom, Signals),
    can_render(Display, Signals, SSD).
can_render(0, Signals, (A, B, C, E, F, G,    % For '0' rendering
                        C, F,                % For '1' rendering
                        A, C, D, E, G,       % For '2' rendering
                        A, C, D, F, G,       % For '3' rendering
                        B, C, D, F,          % For '4' rendering
                        A, B, D, F, G,       % For '5' rendering
                        A, B, D, E, F, G,    % For '6' rendering
                        A, C, F,             % For '7' rendering
                        A, B, C, D, E, F, G, % For '8' rendering
                        A, B, C, D, F, G     % For '9' rendering
                       )) :-
    permutation(Signals, [A,B,C,E,F,G]).
can_render(1, Signals, (A, B, C, E, F, G,    % For '0' rendering
                        C, F,                % For '1' rendering
                        A, C, D, E, G,       % For '2' rendering
                        A, C, D, F, G,       % For '3' rendering
                        B, C, D, F,          % For '4' rendering
                        A, B, D, F, G,       % For '5' rendering
                        A, B, D, E, F, G,    % For '6' rendering
                        A, C, F,             % For '7' rendering
                        A, B, C, D, E, F, G, % For '8' rendering
                        A, B, C, D, F, G     % For '9' rendering
                       )) :-
    permutation(Signals, [C,F]).
can_render(2, Signals, (A, B, C, E, F, G,    % For '0' rendering
                        C, F,                % For '1' rendering
                        A, C, D, E, G,       % For '2' rendering
                        A, C, D, F, G,       % For '3' rendering
                        B, C, D, F,          % For '4' rendering
                        A, B, D, F, G,       % For '5' rendering
                        A, B, D, E, F, G,    % For '6' rendering
                        A, C, F,             % For '7' rendering
                        A, B, C, D, E, F, G, % For '8' rendering
                        A, B, C, D, F, G     % For '9' rendering
                       )) :-
    permutation(Signals, [A,C,D,E,G]).
can_render(3, Signals, (A, B, C, E, F, G,    % For '0' rendering
                        C, F,                % For '1' rendering
                        A, C, D, E, G,       % For '2' rendering
                        A, C, D, F, G,       % For '3' rendering
                        B, C, D, F,          % For '4' rendering
                        A, B, D, F, G,       % For '5' rendering
                        A, B, D, E, F, G,    % For '6' rendering
                        A, C, F,             % For '7' rendering
                        A, B, C, D, E, F, G, % For '8' rendering
                        A, B, C, D, F, G     % For '9' rendering
                       )) :-
    permutation(Signals, [A,C,D,F,G]).
can_render(4, Signals, (A, B, C, E, F, G,    % For '0' rendering
                        C, F,                % For '1' rendering
                        A, C, D, E, G,       % For '2' rendering
                        A, C, D, F, G,       % For '3' rendering
                        B, C, D, F,          % For '4' rendering
                        A, B, D, F, G,       % For '5' rendering
                        A, B, D, E, F, G,    % For '6' rendering
                        A, C, F,             % For '7' rendering
                        A, B, C, D, E, F, G, % For '8' rendering
                        A, B, C, D, F, G     % For '9' rendering
                       )) :-
    permutation(Signals, [B,C,D,F]).
can_render(5, Signals, (A, B, C, E, F, G,    % For '0' rendering
                        C, F,                % For '1' rendering
                        A, C, D, E, G,       % For '2' rendering
                        A, C, D, F, G,       % For '3' rendering
                        B, C, D, F,          % For '4' rendering
                        A, B, D, F, G,       % For '5' rendering
                        A, B, D, E, F, G,    % For '6' rendering
                        A, C, F,             % For '7' rendering
                        A, B, C, D, E, F, G, % For '8' rendering
                        A, B, C, D, F, G     % For '9' rendering
                       )) :-
    permutation(Signals, [A,B,D,F,G]).
can_render(6, Signals, (A, B, C, E, F, G,    % For '0' rendering
                        C, F,                % For '1' rendering
                        A, C, D, E, G,       % For '2' rendering
                        A, C, D, F, G,       % For '3' rendering
                        B, C, D, F,          % For '4' rendering
                        A, B, D, F, G,       % For '5' rendering
                        A, B, D, E, F, G,    % For '6' rendering
                        A, C, F,             % For '7' rendering
                        A, B, C, D, E, F, G, % For '8' rendering
                        A, B, C, D, F, G     % For '9' rendering
                       )) :-
    permutation(Signals, [A,B,D,E,F,G]).
can_render(7, Signals, (A, B, C, E, F, G,    % For '0' rendering
                        C, F,                % For '1' rendering
                        A, C, D, E, G,       % For '2' rendering
                        A, C, D, F, G,       % For '3' rendering
                        B, C, D, F,          % For '4' rendering
                        A, B, D, F, G,       % For '5' rendering
                        A, B, D, E, F, G,    % For '6' rendering
                        A, C, F,             % For '7' rendering
                        A, B, C, D, E, F, G, % For '8' rendering
                        A, B, C, D, F, G     % For '9' rendering
                       )) :-
    permutation(Signals, [A,C,F]).
can_render(8, Signals, (A, B, C, E, F, G,    % For '0' rendering
                        C, F,                % For '1' rendering
                        A, C, D, E, G,       % For '2' rendering
                        A, C, D, F, G,       % For '3' rendering
                        B, C, D, F,          % For '4' rendering
                        A, B, D, F, G,       % For '5' rendering
                        A, B, D, E, F, G,    % For '6' rendering
                        A, C, F,             % For '7' rendering
                        A, B, C, D, E, F, G, % For '8' rendering
                        A, B, C, D, F, G     % For '9' rendering
                       )) :-
    permutation(Signals, [A,B,C,D,E,F,G]).
can_render(9, Signals, (A, B, C, E, F, G,    % For '0' rendering
                        C, F,                % For '1' rendering
                        A, C, D, E, G,       % For '2' rendering
                        A, C, D, F, G,       % For '3' rendering
                        B, C, D, F,          % For '4' rendering
                        A, B, D, F, G,       % For '5' rendering
                        A, B, D, E, F, G,    % For '6' rendering
                        A, C, F,             % For '7' rendering
                        A, B, C, D, E, F, G, % For '8' rendering
                        A, B, C, D, F, G     % For '9' rendering
                       )) :-
    permutation(Signals, [A,B,C,D,F,G]).

render_by(SSD, Atom, Number) :-
    prepare_SSD(SSD, Mapping),
    !,
    render_by(Mapping, Atom, Number).
render_by(Mapping, Atom, Number) :-
    atom(Atom),
    !,
    sort_atom(Atom, Sort),
    findall(N, member(Sort-N,Mapping), [Number]).

prepare_SSD(SSD, [Zero-0,One-1,Two-2,Three-3,Four-4,Five-5,Six-6,Seven-7,Eight-8,Nine-9]) :-
    seven_segments_display(SSD),
    determined_SSD(SSD),
    (A, B, C, E, F, G,    % For '0' rendering
     C, F,                % For '1' rendering
     A, C, D, E, G,       % For '2' rendering
     A, C, D, F, G,       % For '3' rendering
     B, C, D, F,          % For '4' rendering
     A, B, D, F, G,       % For '5' rendering
     A, B, D, E, F, G,    % For '6' rendering
     A, C, F,             % For '7' rendering
     A, B, C, D, E, F, G, % For '8' rendering
     A, B, C, D, F, G     % For '9' rendering
    ) = SSD,
    atom_chars(Z,  [A,B,C,E,F,G]),
    sort_atom(Z, Zero),
    atom_chars(O,   [C,F]),
    sort_atom(O, One),
    atom_chars(T,   [A,C,D,E,G]),
    sort_atom(T, Two),
    atom_chars(Th, [A,C,D,F,G]),
    sort_atom(Th, Three),
    atom_chars(Fo,  [B,C,D,F]),
    sort_atom(Fo, Four),
    atom_chars(Fi,  [A,B,D,F,G]),
    sort_atom(Fi, Five),
    atom_chars(S,   [A,B,D,E,F,G]),
    sort_atom(S, Six),
    atom_chars(Se, [A,C,F]),
    sort_atom(Se, Seven),
    atom_chars(Ei, [A,B,C,D,E,F,G]),
    sort_atom(Ei, Eight),
    atom_chars(N,  [A,B,C,D,F,G]),
    sort_atom(N, Nine).

show_SSD((A, B, C, E, F, G,    % For '0' rendering
          C, F,                % For '1' rendering
          A, C, D, E, G,       % For '2' rendering
          A, C, D, F, G,       % For '3' rendering
          B, C, D, F,          % For '4' rendering
          A, B, D, F, G,       % For '5' rendering
          A, B, D, E, F, G,    % For '6' rendering
          A, C, F,             % For '7' rendering
          A, B, C, D, E, F, G, % For '8' rendering
          A, B, C, D, F, G     % For '9' rendering
         )) :-
    format("(~p~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n)~n",
           [[A,B,C,E,F,G],
            [C,F],
            [A,C,D,E,G],
            [A,C,D,F,G],
            [B,C,D,F],
            [A,B,D,F,G],
            [A,B,D,E,F,G],
            [A,C,F],
            [A,B,C,D,E,F,G],
            [A,B,C,D,F,G]
           ]).

determined_SSD(
    (A, B, C, E, F, G,    % For '0' rendering
     C, F,                % For '1' rendering
     A, C, D, E, G,       % For '2' rendering
     A, C, D, F, G,       % For '3' rendering
     B, C, D, F,          % For '4' rendering
     A, B, D, F, G,       % For '5' rendering
     A, B, D, E, F, G,    % For '6' rendering
     A, C, F,             % For '7' rendering
     A, B, C, D, E, F, G, % For '8' rendering
     A, B, C, D, F, G     % For '9' rendering
    )) :-
    forall(member(T,[A,B,C,D,E,F,G]), not(var(T))).

test_SSD(S) :-
    seven_segments_display(S),
    can_render(0, [a,b,c,e,f,g], S),
    can_render(1, [c,f], S),
    can_render(2, [a,c,d,e,g], S),
    can_render(3, [a,c,d,f,g], S),
    can_render(4, [b,c,d,f], S),
    can_render(5, [a,b,d,f,g], S),
    can_render(6, [a,b,d,e,f,g], S),
    can_render(7, [a,c,f], S),
    can_render(8, [a,b,c,d,e,f,g], S),
    can_render(9, [a,b,c,d,f,g], S),
    show_SSD(S),
    (determined_SSD(S),
     !,
     format("This SSD is determined.");
     format("This SSD is NOT full-determined.")
    ).

solution_2 :-
    data(Input),
    guess_list(Input, Result),
    format("count: ~p~n", [Result]),
    true.
