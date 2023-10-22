%------------------------------------
% Tigran Manukyan
% Class #16842      11am - 12:15pm
% N Queens Project
%------------------------------------


%---------------------------------------
% nc/3
% checkes if there are no conflicts from v in all of P
% V is place of current placement
% P is list of previous placements
% D is distance from V position to the head of P position
%---------------------------------------
nc(V, P, D) :-
    P = [H|T],
    V =\= H,
    D =\= abs(V - H),
    DP1 is D + 1,
    nc(V, T, DP1).

nc(_, [], _).





%---------------------------------------
% solve/2
% calls solve/3 putting [] as the QPT
% N is number of queens
% QP is solution of the N queens problem
%---------------------------------------
solve(N, QP) :-
    solve(N, [], QP).


%---------------------------------------
% solve/3
% calls solve/3 putting [] as the QPT
% N is number of queens
% QPT is current partial solution
% QP is solution of the N queens problem
%---------------------------------------
solve(N, QPT, QP) :-
    length(QPT, L),
    N > L,
    NM1 is N-1,
    between(0, NM1, V),
    nc(V, QPT, 1),
    QPN = [V|QPT],
    solve(N,QPN,QP).

solve(N, QPT, QPT):-
    length(QPT, N).


