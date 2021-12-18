
vs(_, []).
vs(Pv, [Pv|Pvs]) :-
    v(Pv, V1),
    vs(V1, Pvs).

v((Xi,Yi) -> (Xv,Yv), (Xd,Yd) -> (Xv1,Yv1)) :-
    (Xv > 0, !,
     Xv1 is Xv - 1 ;
     Xv < 0, !,
     Xv1 is Xv + 1 ;
     Xv =:= 0, !,
     Xv1 = Xv),
    Yv1 is Yv - 1,
    Xd is Xi + Xv,
    Yd is Yi + Yv.

%% mnx(+Init, +Range, -Velocities)
%  Find both minimum and maximum horizontal velocity.
mnx((Xi,_Yi), Xn -x- Xx, Vxn-Vxx) :-
    Xi =< Xn,
    Dn is Xn - Xi,
    Dx is Xx - Xi,
    once(( between(1, Dn, N1),
           findall(N, between(1,N1,N), Ns),
           sumlist(Ns, S),
           S > Dn,
           length(Ns, Vxn)
         )),
    once(( between(1, Dx, N2),
           findall(N, between(1,N2,N), Ns1),
           sumlist(Ns1, S1),
           S1 > Dx,
           length(Ns1, L),
           Vxx is L - 1
         )).

%% f(+Init, +Velocity, +Range_h, +Rnage_v, -Trajectory, -Result)
%  Find trajectory and result.
f(I, V, Xn -x- Xx, Yn -x- Yx, Vs, Result) :-
    once(( vs(I->V, Vs),
           member((X,Y)->_, Vs),
           (
               Xn =< X, X =< Xx,
               Yn =< Y, Y =< Yx, !,
               Result = hit ;
               X >= Xx, !,
               Result = miss ;
               Y =< Yn, !,
               Result = miss
           )
         )).

n(N, M) :-
    M is N + 1.
n(N, M) :-
    N1 is N + 1,
    n(N1, M).

%h(I, (X,Y), Xn -x- Xx, Yn -x- Yx, Acc, _Result) :-

%% h(+Init, +Velocity_h, +Range_h, +Range_y, +Accumulating, +Highest).
%  Find the way to highest trajectory.
%h(I, X, Xn -x- Xx, Yn -x- Yx, Acc, H) :-

solution :-
    I = (0, 0),
    %Rh = 20 -x- 30,
    Rh = 209 -x- 238,
    %Rv = -10 -x- -5,
    Rv = -86 -x- -59,
    mnx(I, Rh, Xf-Xt),
    findall(Yt,
            (between(Xf, Xt, Xv),
             once(( n(0, Yvn),
                    f(I, (Xv,Yvn), Rh, Rv, _, hit)
                  )),
             once(( n(Yvn, Yvex),
                    f(I, (Xv,Yvex), Rh, Rv, _, miss)
                  )),
             Yvx is Yvex - 1,
             f(I, (Xv,Yvx), Rh, Rv, Tra, _),
             format("~p~n", [Tra]),
             findall(Y, member((_,Y)->_,Tra), Yts),
             max_list(Yts, Yt)
            ),
            Yts),
    max_list(Yts, Yt),
    format("highest position: ~I~n", [Yt]),
    % But this is not a solution.
    %
    % Despite this, both minimun and maximun of vertical velocity
    % can be determined by initial vertical velocity and judged
    % to hit or miss by both lowest and highest poistion of the target.
    %
    % Those trivial things were left in this note.
    true.
