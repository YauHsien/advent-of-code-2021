%% -- Data -----------------------------------
file_path('../inputs/day25_input.txt').

data(Input) :-
    file_path(FilePath),
    csv_read_file(FilePath, Rows),
    input_atom_list(Rows, Input).

input_atom_list([], []).
input_atom_list([row(Atom)|Rows], [Atom|Inputs]) :-
    input_atom_list(Rows, Inputs).

%% sea_cucumber(?Sea_cucumber, ?X, ?Y)
:- dynamic sea_cucumber/3.

:- dynamic top_border/1.
:- dynamic bottom_border/1.
:- dynamic left_border/1.
:- dynamic right_border/1.

%% counter(+Name, ?Count)
:- dynamic counter/2.

update_by(G0, G1) :-
    (
        G0, !,
        retract(G0)
    ;   true
    ),
    assertz(G1).

init(counter, Name) :-
    update_by(counter(Name,_), counter(Name,0)).

tap(counter, Name) :-
    counter(Name, C0),
    C1 is C0+1,
    update_by(counter(Name,C0), counter(Name,C1)).

before((X,Y), '>', (0,Y)) :-
    right_border(X), !.
before((X,Y), 'v', (X,0)) :-
    bottom_border(Y), !.
before((X0,Y), '>', (X,Y)) :-
    X is X0+1.
before((X,Y0), 'v', (X,Y)) :-
    Y is Y0+1.

can_move(Sea_cucumber, X, Y) :-
    sea_cucumber(Sea_cucumber, X, Y),
    before((X,Y), Sea_cucumber, (X1,Y1)),
    \+(sea_cucumber(_,X1,Y1)).

move(Sea_cucumber, X, Y) :-
    sea_cucumber(Sea_cucumber, X, Y),
    can_move(Sea_cucumber, X, Y),
    before((X,Y), Sea_cucumber, (X1,Y1)),
    update_by(sea_cucumber(Sea_cucumber,X,Y), sea_cucumber(Sea_cucumber,X1,Y1)).

move :-
    move('>').
move :-
    move('v').

move(Sea_cucumber) :-
    sea_cucumber(Sea_cucumber, X0, Y0),
    move(Sea_cucumber, X0, Y0).

load(Input) :-
    [A|_] = Input,
    set_width(A),
    set_height(Input),
    retractall(sea_cucumber(_,_,_)),
    load(Input, (0,0)).

set_width(A) :-
    atom_length(A, L),
    B is L -1,
    update_by(left_border(_), left_border(0)),
    update_by(right_border(_), right_border(B)).

set_height(L) :-
    length(L, N),
    B is N -1,
    update_by(top_border(_), top_border(0)),
    update_by(bottom_border(_), bottom_border(B)).

load([], _).
load([Atom|Input], (X0,Y0)) :-
    (
        foreach(
            (
                atom_concat(P, A, Atom),
                atom_prefix(A, A0),
                atom_length(A0, 1),
                atom_length(P, L),
                I0 is X0 +L
            ),
            (
                ( '.' = A0 ; '' = A0 )
            ;   ( '>' = A0 ; 'v' = A0 ),
                assertz(sea_cucumber(A0, I0, Y0))
            )
        )
    ),
    Y is Y0 + 1,
    load(Input, (X0,Y)).

solution :-
    data(Input),
    %trace,
    once(load(Input)),
    %trace,
    init(counter, iteration),
    once((
                %trace,
                move,
                %trace,
                tap(counter, iteration),
                (
                    counter(iteration, It),
                    format("c: ~I~n", [It])
                    %,It =:= 24
                ),
                %trace,
                \+ can_move(_,_,_)
            )),
    trace,
    counter(iteration, Count),
    format("~I steps~n", [Count+1]).
