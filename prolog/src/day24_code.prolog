
%% -- Data -----------------------------------
file_path('../inputs/day24_input.txt').

data(Inputs) :-
    file_path(FilePath),
    csv_read_file(FilePath, Rows, [separator(0'\s),match_arity(false)]),
    input_atom_list(Rows, Inputs).

input_atom_list([], []).
input_atom_list([row('inp',X)|Rows], [(inp,X)|Inputs]) :-
    input_atom_list(Rows, Inputs).
input_atom_list([row(Op,X,Y)|Rows], [(Op,X,Y)|Inputs]) :-
    input_atom_list(Rows, Inputs).

accept(Number) :-
    integer(Number),
    atom_number(Atom, Number),
    atom_length(Atom, 14),
    ( once(( atom_concat(_,A, Atom),
             atom_prefix(A, '0')
           )),
      !,
      fail
    ; true
    ).

:- dynamic slot/2.
slot(w, not_reset).
slot(x, not_reset).
slot(y, not_reset).
slot(z, not_reset).

reset(memory) :-
    retractall(slot(_,_)),
    assertz(slot(w, 0)),
    assertz(slot(x, 0)),
    assertz(slot(y, 0)),
    assertz(slot(z, 0)).

eval(add, X, Y, Z) :-
    Z is X + Y.
eval(mul, X, Y, Z) :-
    Z is X * Y.
eval(div, X, Y, Z) :-
    Z is X div Y.
eval(mod, X, Y, Z) :-
    Z is X mod Y.
eval(eql, X, X, 1).
eval(eql, _, _, 0).

eval([], []) :- !.
%eval([], [(inp,_)|_]) :- !.
eval([Value|Values], [(inp,X)|Instructions]) :-
    %(
    %    slot(z, Z),
    %    format("~I~n", [Z])
    %),
    slot(X, Vx0),
    retract(slot(X,Vx0)),
    parse(int, Value, Vx),
    assertz(slot(X,Vx)),
    eval(Values, Instructions).
eval(Values, [(Op,X,Y)|Instructions]) :-
    slot(X, Vx0),
    (
        atom(Y), !,
        slot(Y, Vy0)
    ;   Vy0 = Y
    ),
    once(eval(Op, Vx0, Vy0, Vx)),
    retract(slot(X,Vx0)),
    assertz(slot(X,Vx)),
    eval(Values, Instructions).

parse(int, I, I) :-
    integer(I), !.
parse(int, A, R) :-
    atom(A),
    atom_number(A, R).

largest(number, from(N), N).
largest(number, from(N), M) :-
    N1 is N-1,
    largest(number, from(N1), M).

model(13_579_246_899_999).

tokens(Number, Tokens) :-
    atom_number(A, Number),
    atom_chars(A, Tokens).

test :-
    data(Cs),
    model(M),
    tokens(M, Ts),
    reset(memory),
    eval(Ts, Cs),
    slot(z, 0).

single(X, Z) :-
    data(Cs),
    A = 1,
    Z = 9,
    X = [B,C,D,E,F,G,H,I,J,K,L,M,N,O],
    between(A, Z, B),
    between(A, Z, C),
    between(A, Z, D),
    between(A, Z, E),
    between(A, Z, F),
    between(A, Z, G),
    between(A, Z, H),
    between(A, Z, I),
    between(A, Z, J),
    between(A, Z, K),
    between(A, Z, L),
    between(A, Z, M),
    between(A, Z, N),
    between(A, Z, O),
    reset(memory),
    eval(X, Cs),
    slot(z, Z).

monad(Number) :-
    accept(Number),
    atom_number(Atom, Number),
    atom_chars(Atom, List0),
    findall(N, (member(A,List0),atom_number(A,N)), List),
    monad(List).
monad([C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14]) :-
    monad(0,   C1,  15,  1,  13, Z1),
    monad(Z1,  C2,  10,  1,  16, Z2),
    monad(Z2,  C3,  12,  1,  2,  Z3),
    monad(Z3,  C4,  10,  1,  8,  Z4),
    monad(Z4,  C5,  14,  1,  11, Z5),
    monad(Z5,  C6,  -11, 26, 6,  Z6),
    convergence(Z5, Z6),
    monad(Z6,  C7,  10,  1,  12, Z7),
    monad(Z7,  C8,  -16, 26, 2,  Z8),
    convergence(Z7, Z8),
    monad(Z8,  C9,  -9,  26, 2,  Z9),
    convergence(Z4, Z9),
    monad(Z9,  C10, 11,  1,  15, Z10),
    monad(Z10, C11, -8,  26, 1,  Z11),
    convergence(Z10, Z11),
    monad(Z11, C12, -8,  26, 10, Z12),
    convergence(Z3, Z12),
    monad(Z12, C13, -10, 26, 14, Z13),
    convergence(Z2, Z13),
    monad(Z13, C14, -9,  26, 10, Z14),
    convergence(Z1, Z14),
    format("~p~n", [((1,Z1),(2,Z2),(3,Z3),(4,Z4),(5,Z5),(6,Z6),(7,Z7),(8,Z8),(9,Z9),(10,Z10),(11,Z11),(12,Z12),(13,Z13),(14,Z14))]),
    Z14 =:= 0.

monad(Z0, W, M, D, _A, Z) :-
    W =:= Z0 mod 26 +M, !,
    Z is Z0 div D.
monad(Z0, W, _M, D, A, Z) :-
    Z is (Z0 div D) *26 +W +A.

convergence(N, M) :-
    N div 26 =:= M.

:- table monad/6.
:- table convergence/2.

solution :-
    %From0 is From - 1,
    %once((
    %            between(0, From0, N),
    %            M is From - N,
    %            format("~I~n", [M]),
    %            monad(M)
    %        )).
    %data(Commands),
    between(1, 11_111_111_111_111, N),
    %M is 55_554_672_721_952 - N,
    M is 53_999_995_899_999 - N,
    %atom_chars(M, Vs),
    once((
                monad(M)
                %reset(memory),
                %eval(Vs, Commands),
                %slot(z, 0)
            )),
    format("~I is largest acceptable model number~n", [M]).

solution_2 :-
    %between(1, 11_111_111_111_111, N),
    %M is 11_921_151_118_376 - N, % This is my guess.
    %between(11_111_111_111_111, 11_921_151_118_375, M),
    L = [1,1,7,2,1,1,A,B,1,1,8,C,D,E],
    between(1, 9, A),
    between(1, 9, B),
    between(1, 9, C),
    between(1, 9, D),
    between(1, 9, E),
    atomic_list_concat(L, L0),
    atom_number(L0, M),
    once((
                %M >= 11_111_111_111_111,
                monad(M)
            )),
    format("~I is smallest acceptable model number~n", [M]).

