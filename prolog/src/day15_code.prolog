
%% -- Data -----------------------------------
file_path('../inputs/day15_input.txt').

data(Input) :-
    file_path(FilePath),
    csv_read_file(FilePath, Rows),
    input_atom_list(Rows, 1, Terms),
    flatten(Terms, Input).

input_atom_list([], _, []).
input_atom_list([Row|Rows], I, [Term|Commands]) :-
    input_term(Row, I, Term),
    I1 is I +1,
    input_atom_list(Rows, I1, Commands).

input_term(Row, Y, Term) :-
    findall(
        (X,Y)-R,
        arg(X, Row, R),
        Term
    ).

%% --- Day 15: Chiton ---
%% You've almost reached the exit of the cave, but the walls are getting closer together. Your submarine can barely still fit, though; the main problem is that the walls of the cave are covered in chitons, and it would be best not to bump any of them.
%
%% The cavern is large, but has a very low ceiling, restricting your motion to two dimensions. The shape of the cavern resembles a square; a quick scan of chiton density produces a map of risk level throughout the cave (your puzzle input). For example:
%
%% 1163751742
%% 1381373672
% 2136511328
%% 3694931569
%% 7463417111
%% 1319128137
%% 1359912421
%% 3125421639
%% 1293138521
%% 2311944581
%% You start in the top left position, your destination is the bottom right position, and you cannot move diagonally. The number at each position is its risk level; to determine the total risk of an entire path, add up the risk levels of each position you enter (that is, don't count the risk level of your starting position unless you enter it; leaving it adds no risk to your total).
%
%% Your goal is to find a path with the lowest total risk. In this example, a path with the lowest total risk is highlighted here:
%
%% 1163751742
%% 1381373672
%% 2136511328
%% 3694931569
%% 7463417111
%% 1319128137
%% 1359912421
%% 3125421639
%% 1293138521
%% 2311944581
%% The total risk of this path is 40 (the starting position is never entered, so its risk is not counted).
%
%% What is the lowest total risk of any path from the top left to the bottom right?

%% vertex(?(X,Y), ?Vertex_as_weight)
:- dynamic vertex/2.

%% select(?(X,Y))
:- dynamic selected/1.

%% weight(?(X0,Y0), ?Weight, ?(X,Y))
:- dynamic weight/3.

reset(load) :-
    retractall(vertex(_,_)).
reset(dijkstra) :-
    retractall(selected(_)),
    retractall(weight(_,_,_)).

update_by(Goal0, Goal) :-
    ( Goal0, !,
      retract(Goal0)
    ; true
    ),
    assertz(Goal).

load(Data) :-
    reset(load),
    foreach(
        member((X,Y)-V, Data),
        assertz(vertex((X,Y),V))
    ).

:- table neighbor/2.

%neighbor((X0,Y0), (X,Y0)) :-
%    X is X0 -1,
%    vertex((X,Y0), _).
neighbor((X0,Y0), (X,Y0)) :-
    X is X0 +1,
    vertex((X,Y0), _).
%neighbor((X0,Y0), (X0,Y)) :-
%    Y is Y0 -1,
%    vertex((X0,Y), _).
neighbor((X0,Y0), (X0,Y)) :-
    Y is Y0 +1,
    vertex((X0,Y), _).

%% select(-(X,Y))
select(Xy) :-
    findall(
        W-Xy,
        (
            vertex(Xy, _),
            \+ selected(Xy),
            weight(_, W, Xy)
        ),
        Rs
    ),
    %format("r: ~p~n", [Rs]),
    sort(Rs, [W-Xy|_]),
    (
        %trace,
        %format("~p~n", [Xy]),
        assertz(selected(Xy))
    ).

release(Xy) :-
    foreach(
        (
            neighbor(Xy, Xy1),
            \+ selected(Xy1)
        ),
        release(Xy, Xy1)
    ).

release((X0,Y0), (X1,Y1)) :-
    (
        weight(S, W0, (X0,Y0)),
        vertex((X1,Y1), W1),
        W is W0 +W1
    ),
    ( weight(S, W2, (X1,Y1)), !,
      ( W < W2, !,
        %trace,
        update_by(weight(S,W2,(X1,Y1)), weight(S,W,(X1,Y1)))
      ; true
      )
    ; %trace,
      assertz(weight(S,W,(X1,Y1)))
    ).

dijkstra(_From_xy, To_xy) :-
    once((
                repeat,
                %trace,
                select(Xy0),
                %trace,
                release(Xy0),
                %trace,
                selected(To_xy)
            )).

solution :-
    data(Input),
    load(Input),
    (
        [From_xy-_|_] = Input,
        reverse(Input, [To_xy-_|_])
    ),
    reset(dijkstra),
    assertz(weight(From_xy,0,From_xy)),
    dijkstra(From_xy, To_xy),
    weight(From_xy, Weight, To_xy),
    format("lowest risk level: ~I~n", [Weight]).

load5(Data, (W,H)) :-
    reset(load),
    W0 is W div 5,
    H0 is H div 5,
    foreach(
        (
            member((X,Y)-V, Data),
            X1 is X +W0,
            X2 is X1 +W0,
            X3 is X2 +W0,
            X4 is X3 +W0,
            Y1 is Y +H0,
            Y2 is Y1 +H0,
            Y3 is Y2 +H0,
            Y4 is Y3 +H0,
            member(X0, [X,X1,X2,X3,X4]),
            member(Y0, [Y,Y1,Y2,Y3,Y4]),
            V0 is ((X0-X+1) div W0 + (Y0-Y+1) div H0 +V -1) mod 9 + 1
        ),
        assertz(vertex((X0,Y0),V0))
    ).

solution_2 :-
    data(Input),
    (
        [From_xy-_|_] = Input,
        reverse(Input, [(X0,Y0)-_|_]),
        W is X0 *5,
        H is Y0 *5,
        To_xy = (W,H)
    ),
    load5(Input, (W,H)),
    reset(dijkstra),
    assertz(weight(From_xy,0,From_xy)),
    dijkstra(From_xy, To_xy),
    weight(From_xy, Weight, To_xy),
    format("lowest risk level: ~I~n", [Weight]).

layout(W,H) :-
    P = foreach(
            (
                between(1, W, X),
                vertex((X,Y), V)
            ),
            format("~I", [V])
        ),
    foreach(between(1,H,Y), (call(P),format("~n"))).
