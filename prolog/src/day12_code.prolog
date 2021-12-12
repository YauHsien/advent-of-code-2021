:- dynamic link/2.

%% -- Data -----------------------------------
file_path('../inputs/day12_input.txt').

data(Inputs) :-
    file_path(FilePath),
    csv_read_file(FilePath, Rows, [convert(false)]),
    input_atom_list(Rows, Inputs).

input_atom_list([], []).
input_atom_list([row(Atom)|Rows], [Term|Inputs]) :-
    input_term(Atom, Term),
    input_atom_list(Rows, Inputs).

input_term(Atom, link(X,Y)) :-
    atom_concat(X, Y1, Atom),
    atom_prefix(Y1, '-'),
    atom_concat('-', Y, Y1).

load_data(Data) :-
    retractall(link(_,_)),
    foreach(member(link(X,Y), Data),
            ((link(X, Y),
              !;
              assertz(link(X,Y))
             ),
             (link(X, Y),
              format("loaded ~p.~n", [link(X,Y)]),
              !;
              true
             )
            )
           ),
    data_conclusion.

data_conclusion :-
    findall(link(X,Y), link(X,Y), Data),
    length(Data, Length),
    format("~I links.~n", [Length]).

%% --- Day 12: Passage Pathing ---
%With your submarine's subterranean subsystems subsisting suboptimally, the only way you're getting out of this cave anytime soon is by finding a path yourself. Not just a path - the only way to know if you've found the best path is to find all of them.
%
%Fortunately, the sensors are still mostly working, and so you build a rough map of the remaining caves (your puzzle input). For example:
%
%start-A
%start-b
%A-c
%A-b
%b-d
%A-end
%b-end
%This is a list of how all of the caves are connected. You start in the cave named start, and your destination is the cave named end. An entry like b-d means that cave b is connected to cave d - that is, you can move between them.
%
%So, the above cave system looks roughly like this:
%
%    start
%    /   \
%c--A-----b--d
%    \   /
%     end
%Your goal is to find the number of distinct paths that start at start, end at end, and don't visit small caves more than once. There are two types of caves: big caves (written in uppercase, like A) and small caves (written in lowercase, like b). It would be a waste of time to visit any small cave more than once, but big caves are large enough that it might be worth visiting them multiple times. So, all paths you find should visit small caves at most once, and can visit big caves any number of times.
%
%Given these rules, there are 10 paths through this example cave system:
%
%start,A,b,A,c,A,end
%start,A,b,A,end
%start,A,b,end
%start,A,c,A,b,A,end
%start,A,c,A,b,end
%start,A,c,A,end
%start,A,end
%start,b,A,c,A,end
%start,b,A,end
%start,b,end
%(Each line in the above list corresponds to a single path; the caves visited by that path are listed in the order they are visited and separated by commas.)
%
%Note that in this cave system, cave d is never visited by any path: to do so, cave b would need to be visited twice (once on the way to cave d and a second time when returning from cave d), and since cave b is small, this is not allowed.
%
%Here is a slightly larger example:
%
%dc-end
%HN-start
%start-kj
%dc-start
%dc-HN
%LN-dc
%HN-end
%kj-sa
%kj-HN
%kj-dc
%The 19 paths through it are as follows:
%
%start,HN,dc,HN,end
%start,HN,dc,HN,kj,HN,end
%start,HN,dc,end
%start,HN,dc,kj,HN,end
%start,HN,end
%start,HN,kj,HN,dc,HN,end
%start,HN,kj,HN,dc,end
%start,HN,kj,HN,end
%start,HN,kj,dc,HN,end
%start,HN,kj,dc,end
%start,dc,HN,end
%start,dc,HN,kj,HN,end
%start,dc,end
%start,dc,kj,HN,end
%start,kj,HN,dc,HN,end
%start,kj,HN,dc,end
%start,kj,HN,end
%start,kj,dc,HN,end
%start,kj,dc,end
%Finally, this even larger example has 226 paths through it:
%
%fs-end
%he-DX
%fs-he
%start-DX
%pj-DX
%end-zg
%zg-sl
%zg-pj
%pj-he
%RW-he
%fs-DX
%pj-RW
%zg-RW
%start-pj
%he-WI
%zg-he
%pj-fs
%start-RW
%How many paths through this cave system are there that visit small caves at most once?

path([], Path) :-
    edge(X, end),
    path([edge(X,end)], Path).
path([edge(X,Y)|Acc], Path) :-
    end \= X,
    edge(Z, X),
    (start = Z,
     Path = [edge(Z,X),edge(X,Y)|Acc];
     start \= Z,
     small(Z),
     not((member(edge(_,Z),[edge(X,Y)|Acc]),
          member(edge(Z,_),[edge(X,Y)|Acc]))),
     %format("~p~p~n", [edge(Z,X),Acc]),
     path([edge(Z,X),edge(X,Y)|Acc], Path);
     start \= Z,
     big(Z),
     path([edge(Z,X),edge(X,Y)|Acc], Path)
    ).

edge(X, Y) :-
    link(X, Y).
edge(X, Y) :-
    link(Y, X).

big_cases(Cases) :-
    atom_chars('ABCDEFGHIJKLMNOPQRSTUVWXYZ', Cases).

small_cases(Cases) :-
    atom_chars('abcdefghijklmnopqrstuvwxyz', Cases).

big(X) :-
    atom_prefix(X, C),
    atom_length(C, 1),
    big_cases(Cases),
    member(C, Cases).

small(X) :-
    atom_prefix(X, C),
    atom_length(C, 1),
    small_cases(Cases),
    member(C, Cases).

solution :-
    data(Input),
    load_data(Input),
    findall(Path,
            path([],Path),
            Paths),
    foreach(member(Path, Paths),
            format("path ~p~n", [Path])),
    length(Paths, Length),
    format("~I paths~n", [Length]),
    true.

%% --- Part Two ---
%% After reviewing the available paths, you realize you might have time to visit a single small cave twice. Specifically, big caves can be visited any number of times, a single small cave can be visited at most twice, and the remaining small caves can be visited at most once. However, the caves named start and end can only be visited exactly once each: once you leave the start cave, you may not return to it, and once you reach the end cave, the path must end immediately.
%
%% Now, the 36 possible paths through the first example above are:
%
%% start,A,b,A,b,A,c,A,end
%% start,A,b,A,b,A,end
%% start,A,b,A,b,end
%% start,A,b,A,c,A,b,A,end
%% start,A,b,A,c,A,b,end
%% start,A,b,A,c,A,c,A,end
%% start,A,b,A,c,A,end
%% start,A,b,A,end
%% start,A,b,d,b,A,c,A,end
%% start,A,b,d,b,A,end
%% start,A,b,d,b,end
%% start,A,b,end
%% start,A,c,A,b,A,b,A,end
%% start,A,c,A,b,A,b,end
%% start,A,c,A,b,A,c,A,end
%% start,A,c,A,b,A,end
%% start,A,c,A,b,d,b,A,end
%% start,A,c,A,b,d,b,end
%% start,A,c,A,b,end
%% start,A,c,A,c,A,b,A,end
%% start,A,c,A,c,A,b,end
%% start,A,c,A,c,A,end
%% start,A,c,A,end
%% start,A,end
%% start,b,A,b,A,c,A,end
%% start,b,A,b,A,end
%% start,b,A,b,end
%% start,b,A,c,A,b,A,end
%% start,b,A,c,A,b,end
%% start,b,A,c,A,c,A,end
%% start,b,A,c,A,end
%% start,b,A,end
%% start,b,d,b,A,c,A,end
%% start,b,d,b,A,end
%% start,b,d,b,end
%% start,b,end
%% The slightly larger example above now has 103 paths through it, and the even larger example now has 3509 paths through it.
%
%% Given these new rules, how many paths through this cave system are there?

path_2([], Path) :-
    edge(start, X),
    path_2([edge(start,X)], Path).
path_2([Edge|Acc], Path) :-
    edge(_,X) = Edge,
    start \= X,
    edge(X, Z),
    start \= Z,
    New = edge(X,Z),
    (end = Z,
     reverse([New,Edge|Acc],Path);
     end \= Z,
     (big(Z),
      path_2([New,Edge|Acc], Path);
      small(Z),
      (other_twice(Z, [New,Edge|Acc]),
       me_absent(Z, [Edge|Acc]),
       path_2([New,Edge|Acc], Path);
       not(other_twice(Z, [New,Edge|Acc])),
       (me_once(Z, [Edge|Acc]);
        me_absent(Z, [Edge|Acc])),
       path_2([New,Edge|Acc], Path)
       %,!;format("FOMO: ~p~n", [[Z, New, Edge|Acc]])
      )
     )
    ).

other_twice(X, List) :-
    %% Caves, other than X, in the List, which is
    %% passed twice.
    findall(E,
            (append(_, [edge(E,_),edge(_,E)|_], List),
             small(E),
             E \= X
            ),
            L_0),
    msort(L_0, L_1),
    clumped(L_1, L_2),
    %format("other twice: ~p ~p ~p~n", [X, List, L_2]),
    member(_X-2, L_2),
    %format("other twice OK: ~p ~p ~p ~p~n", [X,_X,List,L_2]),
    !.

me_once(X, List) :-
    findall(E,
            (append(_, [edge(E,_),edge(_,E)|_], List),
             E == X
            ),
            L_0),
    msort(L_0, L_1),
    clumped(L_1, L_2),
    %format("me once: ~p ~p ~p~n", [X, List, L_2]),
    (member(X-1, L_2);
     [] = L_2),
    %format("me once OK: ~p ~p ~p ~n", [X,List,L_2]),
    !.

me_absent(X, List) :-
    findall(E,
            (append(_, [edge(E,_),edge(_,E)|_], List),
             E == X
            ),
            L_0),
    msort(L_0, L_1),
    clumped(L_1, L_2),
    %format("me absent: ~p ~p ~p~n", [X,List,L_2]),
    (member(X-0, L_2);
     [] = L_2),
    %format("me absent OK: ~p ~p ~p ~n", [X,List,L_2]),
    !.

solution_2 :-
    data(Input),
    load_data(Input),
    findall(Path,
            path_2([],Path),
            P_1),
    sort(P_1, Paths),
    foreach((member(Path, Paths),
             findall(E,append(_,[edge(_,E),edge(E,_)|_],Path),L1)),
            format("path ~p~n", [L1])),
    length(Paths, Length),
    format("~I paths~n", [Length]),
    true.
