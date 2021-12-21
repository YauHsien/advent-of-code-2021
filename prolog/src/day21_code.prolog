
roll(X, Y, Z, d(A,A,B,N), d(C,A,B,M)) :-
    X = A,
    Y is A+1-B,
    Z is Y+1,
    C is Z+1,
    M is N+3.
roll(X, Y, Z, d(A,A,B,N), d(C,A,B,M)) :- !,
    X = A,
    Y is A+1-B,
    Z is Y+1,
    C is Z+1,
    M is N+3.
roll(X, Y, Z, d(A,B,C,N), d(D,B,C,M)) :-
    B-A =:= 1, !,
    X = A,
    Y is X+1,
    Z is B+1-C,
    D is Z+1,
    M is N+3.
roll(X, Y, Z, d(A,B,C,N), d(D,B,C,M)) :-
    X = A,
    Y is X+1,
    Z is Y+1,
    D is Z+1,
    M is N+3.

check(N, L, d(_,_,_,M)) :-
    N >= 1000,
    format("Winning score: ~I~n", [N]),
    format("~I = ~I * ~I~n", [L*M,L,M]).

round(P1-S1, P2-S2, Dice) :-
    once(roll(X1, Y1, Z1, Dice, D1)),
    P1_ is (X1+Y1+Z1+P1-1) rem  10 + 1,
    S1_ is P1_ + S1,
    format("Player 1 rolls ~p and moves to space ~I fro a total score of ~I~n", [(X1,Y1,Z1),P1_,S1_]),
    ( check(S1_, S2, D1), ! ;
      once(roll(X2, Y2, Z2, D1, D2)),
      P2_ is (X2+Y2+Z2+P2-1) rem 10 + 1,
      S2_ is P2_ + S2,
      format("Player 2 rolls ~p and moves to space ~I fro a total score of ~I~n", [(X2,Y2,Z2),P2_,S2_]),
      ( check(S2_, S1_, D2), ! ;
        round(P1_-S1_, P2_-S2_, D2)
      )
    ).

dice(d(1,100,100,0)).

solution :-
    dice(Dice),
    %round(4-0, 8-0, Dice),
    round(3-0, 10-0, Dice).
