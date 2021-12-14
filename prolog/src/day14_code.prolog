
%% -- Data -----------------------------------
file_path('../inputs/day14_input.txt').

data(Inputs) :-
    file_path(FilePath),
    csv_read_file(FilePath, Rows, [separator(0'\s),convert(false),match_arity(false)]),
    input_atom_list(Rows, Inputs).

input_atom_list([], _Polymer-[]).
input_atom_list([row('')|Rows], Polymer-Inputs) :-
    !,
    input_atom_list(Rows, Polymer-Inputs).
input_atom_list([row(Atom)|Rows], Polymer-Inputs) :-
    input_term(row(Atom), Polymer),
    input_atom_list(Rows, Polymer-Inputs).
input_atom_list([Row|Rows], Polymer-[Term|Inputs]) :-
    row(_,_,_) = Row,
    input_term(Row, Term),
    input_atom_list(Rows, Polymer-Inputs).

input_term(row(Atom), Term) :-
    atom_chars(Atom, List),
    clumped(List, Term).
input_term(row(Src,'->',Snk), [X,Y]-Snk) :-
    atom_concat(X, Y, Src),
    '' \= X,
    '' \= Y.

%% --- Day 14: Extended Polymerization ---
%% The incredible pressures at this depth are starting to put a strain on your submarine. The submarine has polymerization equipment that would produce suitable materials to reinforce the submarine, and the nearby volcanically-active caves should even have the necessary input elements in sufficient quantities.
%
%% The submarine manual contains instructions for finding the optimal polymer formula; specifically, it offers a polymer template and a list of pair insertion rules (your puzzle input). You just need to work out what polymer would result after repeating the pair insertion process a few times.
%
%% For example:
%
%% NNCB
%
%% CH -> B
%% HH -> N
%% CB -> H
%% NH -> C
%% HB -> C
%% HC -> B
%% HN -> C
%% NN -> C
%% BH -> H
%% NC -> B
%% NB -> B
%% BN -> B
%% BB -> N
%% BC -> B
%% CC -> N
%% CN -> C
%% The first line is the polymer template - this is the starting point of the process.
%
%% The following section defines the pair insertion rules. A rule like AB -> C means that when elements A and B are immediately adjacent, element C should be inserted between them. These insertions all happen simultaneously.
%
%% So, starting with the polymer template NNCB, the first step simultaneously considers all three pairs:
%
%% The first pair (NN) matches the rule NN -> C, so element C is inserted between the first N and the second N.
%% The second pair (NC) matches the rule NC -> B, so element B is inserted between the N and the C.
%% The third pair (CB) matches the rule CB -> H, so element H is inserted between the C and the B.
%% Note that these pairs overlap: the second element of one pair is the first element of the next pair. Also, because all pairs are considered simultaneously, inserted elements are not considered to be part of a pair until the next step.
%
%% After the first step of this process, the polymer becomes NCNBCHB.
%
%% Here are the results of a few steps using the above rules:
%
%% Template:     NNCB
%% After step 1: NCNBCHB
%% After step 2: NBCCNBBBCBHCB
%% After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
%% After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
%% This polymer grows quickly. After step 5, it has length 97; After step 10, it has length 3073. After step 10, B occurs 1749 times, C occurs 298 times, H occurs 191 times, and N occurs 865 times; taking the quantity of the most common element (B, 1749) and subtracting the quantity of the least common element (H, 161) produces 1749 - 161 = 1588.
%
%% Apply 10 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?

insert([X-1], _Rules, [X-1]).
insert([X-N,Y-M|Rest], Rules, [X-2|Polymer]) :-
    1 =:= N,
    member([X,Y]-X, Rules),
    insert([Y-M|Rest], Rules, Polymer).
insert([X-N,Y-M|Rest], Rules, [X-1,Y-(1+K)|Polymer]) :-
    1 =:= N,
    member([X,Y]-Y, Rules),
    insert([Y-M|Rest], Rules, [Y-K|Polymer]).
insert([X-N,Y-M|Rest], Rules, [X-1,Z-1|Polymer]) :-
    1 =:= N,
    member([X,Y]-Z, Rules),
    X \= Z,
    Y \= Z,
    insert([Y-M|Rest], Rules, Polymer).
insert([X-N|Rest], Rules, [X-(N+N-1)|Polymer]) :-
    1 < N,
    member([X,X]-X, Rules),
    insert([X-1|Rest], Rules, [X-1|Polymer]).
insert([X-N|Rest], Rules, Polymer) :-
    1 < N,
    member([X,X]-Y, Rules),
    X \= Y,
    N_1 is N - 1,
    findall([X-1,Y-1],
            between(1, N_1, _),
            Cross),
    flatten(Cross, Head),
    insert([X-1|Rest], Rules, Rest_1),
    append(Head, Rest_1, Polymer).

apply(Times, Polymer, Rules, Result) :-
    0 < Times,
    !,
    %format("~I ~p~n", [Times,Polymer]),
    insert(Polymer, Rules, Polymer_1),
    apply(Times-1, Polymer_1, Rules, Result).
apply(_, Polymer, _, Polymer).

unclumped(Clumped, Unclumped) :-
    findall(Os,
            (member(E-N1, Clumped),
             N is N1,
             findall(E, (between(1,N,_)), Os)
            ),
            C),
    flatten(C, Unclumped).

polymer_length(Polymer, Len) :-
    unclumped(Polymer, U),
    length(U, Len).

show(Polymer) :-
    unclumped(Polymer, U),
    atom_chars(A, U),
    format("~a~n", [A]).

inspect :-
    data(P-Rs),
    show(P),
    foreach((between(1,10,N),
             apply(N,P,Rs,R),
             polymer_length(R,L)),
            (show(R),
             format("~I~n",[L]))).

inspect(Polymer) :-
    msort(Polymer, S),
    unclumped(S, U),
    clumped(U, C),
    predsort(sort_pred, C, R),
    append([Sm-Nsm|_], [L-Nl], R),
    show(Polymer),
    format("min: ~I ~p(s); max: ~I ~ps (diff: ~I)~n", [Nsm,Sm,Nl,L,Nl-Nsm]).

sort_pred('=', _-A, _-B) :-
    A =:= B.
sort_pred('<', _-A, _-B) :-
    A < B.
sort_pred('>', _-A, _-B) :-
    A > B.

solution :-
    data(P-Rs),
    apply(10, P, Rs, R),
    inspect(R).
