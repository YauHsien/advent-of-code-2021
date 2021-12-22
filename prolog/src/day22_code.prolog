
%% -- Data -----------------------------------
file_path('../inputs/day22_input.txt').

data(Inputs) :-
    file_path(FilePath),
    csv_read_file(FilePath, Rows, [convert(false)]),
    input_atom_list(Rows, Inputs).

input_atom_list([], []).
input_atom_list([row(Ax,Ay,Az)|Rows], [cuboid(S,Xn-Xx,Yn-Yx,Zn-Zx)|Inputs]) :-
    input_term(Ax, Ay, Az, S, Xn-Xx, Yn-Yx, Zn-Zx),
    input_atom_list(Rows, Inputs).

input_term(Ax, Ay, Az, S, Xn-Xx, Yn-Yx, Zn-Zx) :-
    atomic_list_concat([S,X0], ' x=', Ax),
    atomic_list_concat(['',Y0], 'y=', Ay),
    atomic_list_concat(['',Z0], 'z=', Az),
    atomic_list_concat([Xn0,Xx0], '..', X0),
    atomic_list_concat([Yn0,Yx0], '..', Y0),
    atomic_list_concat([Zn0,Zx0], '..', Z0),
    atom_number(Xn0, Xn),
    atom_number(Xx0, Xx),
    atom_number(Yn0, Yn),
    atom_number(Yx0, Yx),
    atom_number(Zn0, Zn),
    atom_number(Zx0, Zx).

lbound(-50).

ubound(50).

opposite(on, off).
opposite(off, on).
opposite([], []).
opposite([cube(State,X,Y,Z)|Cubes], [cube(State1,X,Y,Z)|Cubes1]) :-
    opposite(State, State1),
    opposite(Cubes, Cubes1).

fit(lbound, N, M) :-
    lbound(X),
    M is max(N,X).
fit(ubound, N, M) :-
    ubound(X),
    M is min(N,X).

normal(A-B, B-A) :-
    B < A, !.
normal(A-B, A-B).

inspect(List) :-
    length(List, Len),
    format("length ~I~n", [Len]).

cuboid(State, Xn-Xm, Yn-Ym, Zn-Zm, Cubes) :-
    normal(Xn-Xm, Xn1-Xm1),
    normal(Yn-Ym, Yn1-Ym1),
    normal(Zn-Zm, Zn1-Zm1),
    fit(lbound, Xn1, Xn2),
    fit(lbound, Yn1, Yn2),
    fit(lbound, Zn1, Zn2),
    fit(ubound, Xm1, Xm2),
    fit(ubound, Ym1, Ym2),
    fit(ubound, Zm1, Zm2),
    trace,
    findall(cube(State, X, Y, Z),
            (
                between(Xn2, Xm2, X),
                between(Yn2, Ym2, Y),
                between(Zn2, Zm2, Z)
            ),
            Cubes).

cuboid([], Result, Result).
cuboid([Command|Commands], Acc, Result) :-
    cuboid(_,_-_,_-_,_-_,Cubes) = Command, !,
    Command,
    trace,
    opposite(Cubes, Os),
    subtract(Acc, Os, Rs),
    trace,
    inspect(Cubes),
    inspect(Rs),
    union(Cubes, Rs, Acc1),
    trace,
    format("acc: ~p~ncubes: ~p~nremain: ~p~n", [Acc,Cubes,Rs]),
    trace,
    cuboid(Commands, Acc1, Result).

test :-
    %% on x=10..12,y=10..12,z=10..12
    %% on x=11..13,y=11..13,z=11..13
    %% off x=9..11,y=9..11,z=9..11
    %% on x=10..10,y=10..10,z=10..10
    Cs = [cuboid(on, 10-12, 10-12, 10-12, _),
          cuboid(on, 11-13, 11-13, 11-13, _),
          cuboid(off, 9-11, 9-11, 9-11, _),
          cuboid(on, 10-10, 10-10, 10-10, _)
         ],
    cuboid(Cs, [], Result),
    %length(Result, N),
    findall(X, (member(X,Result),cube(on,_,_,_)=X), Os),
    length(Os, Nos),
    %format("~I ~p~n", [N,Result]),
    format("~I ~p~n", [Nos,Os]).

test_1 :-
    %% on x=-20..26,y=-36..17,z=-47..7
    %% on x=-20..33,y=-21..23,z=-26..28
    %% on x=-22..28,y=-29..23,z=-38..16
    %% on x=-46..7,y=-6..46,z=-50..-1
    %% on x=-49..1,y=-3..46,z=-24..28
    %% on x=2..47,y=-22..22,z=-23..27
    %% on x=-27..23,y=-28..26,z=-21..29
    %% on x=-39..5,y=-6..47,z=-3..44
    %% on x=-30..21,y=-8..43,z=-13..34
    %% on x=-22..26,y=-27..20,z=-29..19
    %% off x=-48..-32,y=26..41,z=-47..-37
    %% on x=-12..35,y=6..50,z=-50..-2
    %% off x=-48..-32,y=-32..-16,z=-15..-5
    %% on x=-18..26,y=-33..15,z=-7..46
    %% off x=-40..-22,y=-38..-28,z=23..41
    %% on x=-16..35,y=-41..10,z=-47..6
    %% off x=-32..-23,y=11..30,z=-14..3
    %% on x=-49..-5,y=-3..45,z=-29..18
    %% off x=18..30,y=-20..-8,z=-3..13
    %% on x=-41..9,y=-7..43,z=-33..15
    %% on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
    %% on x=967..23432,y=45373..81175,z=27513..53682
    Cs = [
        cuboid(on, (-20)-26, (-36)-17, (-47)-7, _),
        cuboid(on, (-20)-33, (-21)-23, (-26)-28, _),
        cuboid(on, (-22)-28, (-29)-23, (-38)-16, _),
        cuboid(on, (-46)-7, (-6)-46, (-50)-(-1), _),
        cuboid(on, (-49)-1, (-3)-46, (-24)-28, _),
        cuboid(on, 2-47, (-22)-22, (-23)-27, _),
        cuboid(on, (-27)-23, (-28)-26, (-21)-29, _),
        cuboid(on, (-39)-5, (-6)-47, (-3)-44, _),
        cuboid(on, (-30)-21, (-8)-43, (-13)-34, _),
        cuboid(on, (-22)-26, (-27)-20, (-29)-19, _),
        cuboid(off, (-48)-(-32), 26-41, (-47)-(-37), _),
        cuboid(on, (-12)-35, 6-50, (-50)-(-2), _),
        cuboid(off, (-48)-(-32), (-32)-(-16), (-15)-(-5), _),
        cuboid(on, (-18)-26, (-33)-15, (-7)-46, _),
        cuboid(off, (-40)-(-22), (-38)-(-28), 23-41, _),
        cuboid(on, (-16)-35, (-41)-10, (-47)-6, _),
        cuboid(off, (-32)-(-23), 11-30, (-14)-3, _),
        cuboid(on, (-49)-(-5), (-3)-45, (-29)-18, _),
        cuboid(off, 18-30, (-20)-(-8), (-3)-13, _),
        cuboid(on, (-41)-9, (-7)-43, (-33)-15, _),
        cuboid(on, (-54112)-(-39298), (-85059)-(-49293), (-27449)-7877, _),
        cuboid(on, 967-23432, 45373-81175, 27513-53682, _)
    ],
    cuboid(Cs, [], Result),
    %length(Result, N),
    findall(X, (member(X,Result),cuboid(on,_,_,_)=X), Os),
    length(Os, Nos),
    %format("~I ~p~n", [N,Result]),
    format("~I ~p~n", [Nos,Os]).

normal(Xn-Xm, Yn-Ym, Zn-Zm, Xn1-Xm1, Yn1-Ym1, Zn1-Zm1) :-
    normal(Xn-Xm, Xn1-Xm1),
    normal(Yn-Ym, Yn1-Ym1),
    normal(Zn-Zm, Zn1-Zm1).

cube(State, (X,Y,Z), [cuboid(State1,Xn-Xm,Yn-Ym,Zn-Zm)|_]) :-
    between(Xn, Xm, X),
    between(Yn, Ym, Y),
    between(Zn, Zm, Z), !,
    ( ground(State), !,
      State == State1
    ; State = State1
    ).
cube(State, Xyz, [_|Cuboids]) :-
    cube(State, Xyz, Cuboids).

solution :-
    lbound(N),
    ubound(M),
    %% -- Inut ----------
    %% Small example
    %Cs0 = [cuboid(on, 10-12, 10-12, 10-12),
    %       cuboid(on, 11-13, 11-13, 11-13),
    %       cuboid(off, 9-11, 9-11, 9-11),
    %       cuboid(on, 10-10, 10-10, 10-10)
    %      ],
    %% Large example
    %Cs0 = [
    %    cuboid(on, (-20)-26, (-36)-17, (-47)-7),
    %    cuboid(on, (-20)-33, (-21)-23, (-26)-28),
    %    cuboid(on, (-22)-28, (-29)-23, (-38)-16),
    %    cuboid(on, (-46)-7, (-6)-46, (-50)-(-1)),
    %    cuboid(on, (-49)-1, (-3)-46, (-24)-28),
    %    cuboid(on, 2-47, (-22)-22, (-23)-27),
    %    cuboid(on, (-27)-23, (-28)-26, (-21)-29),
    %    cuboid(on, (-39)-5, (-6)-47, (-3)-44),
    %    cuboid(on, (-30)-21, (-8)-43, (-13)-34),
    %    cuboid(on, (-22)-26, (-27)-20, (-29)-19),
    %    cuboid(off, (-48)-(-32), 26-41, (-47)-(-37)),
    %    cuboid(on, (-12)-35, 6-50, (-50)-(-2)),
    %    cuboid(off, (-48)-(-32), (-32)-(-16), (-15)-(-5)),
    %    cuboid(on, (-18)-26, (-33)-15, (-7)-46),
    %    cuboid(off, (-40)-(-22), (-38)-(-28), 23-41),
    %    cuboid(on, (-16)-35, (-41)-10, (-47)-6),
    %    cuboid(off, (-32)-(-23), 11-30, (-14)-3),
    %    cuboid(on, (-49)-(-5), (-3)-45, (-29)-18),
    %    cuboid(off, 18-30, (-20)-(-8), (-3)-13),
    %    cuboid(on, (-41)-9, (-7)-43, (-33)-15),
    %    cuboid(on, (-54112)-(-39298), (-85059)-(-49293), (-27449)-7877),
    %    cuboid(on, 967-23432, 45373-81175, 27513-53682)
    %],
    %% Load from data
    data(Cs0),
    %% -- End of input ------
    reverse(Cs0, Cs1),
    findall(
        cuboid(S,X1,Y1,Z1),
        (
            member(cuboid(S,X,Y,Z), Cs1),
            normal(X, Y, Z, X1, Y1, Z1)
        ),
        Cs),
    %format("~p~n",[Cs]),
    findall((X,Y,Z),
            (
                between(N, M, X),
                between(N, M, Y),
                between(N, M, Z),
                cube(on, (X,Y,Z), Cs)
            ),
            Found
           ),
    length(Found, Len),
    format("~I found~n", [Len]),
    %format("~p~n", [Found]),
    true.
