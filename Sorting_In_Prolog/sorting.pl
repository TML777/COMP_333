%------------------------------------
% Tigran Manukyan
% Class #16842      11am - 12:15pm
% Prolog Sorting Programming Project: selection sort and merge sort
%------------------------------------



%--------------------------------------
% support predicates rv/2, rl/3, is_sorted/1
%--------------------------------------

%---------------------------------------
% rv/2
% creats random value V from range 0 to R-1
% R defines range
% V is random value from range 0 to R-1
%---------------------------------------
rv(R,V) :-
    random(W),
    V is floor(W*R).


%---------------------------------------
% rl/3
% creates N random numbers in list L from range 0 to R-1
% N length of list
% R defines range
% L is list(length N) of random value from range 0 to R-1
%---------------------------------------
rl(N,R,L):-
    length(L,N),
    maplist(rv(R),L).


%---------------------------------------
% is_sorted/1
% checkes if list L is sorted
% L is list
%---------------------------------------
is_sorted(L) :-
    length(L,LL),
    LL > 1,
    L = [LH1|LT1],
    LT1 = [LH2|_],
    LH1 =< LH2,
    is_sorted(LT1).

is_sorted([_]).

is_sorted([]).





%---------------------------------------
% Selection Sort Predicates: swap/4, min_index/3, selection_sort/2
%---------------------------------------


%---------------------------------------
% swap/4
% swaps values at index A and B of list L and puts new list in M
% L is list
% A is first index to be swapped
% B is second index to be swapped
% M is list with elempents at A and B swapped
%---------------------------------------
swap(L, A, B, M) :-
    nth0(A, L, AV),
    nth0(B, L, BV),
    nth0(A, L, _, T1),
    nth0(A, T2, BV, T1),
    nth0(B, T2, _, T3),
    nth0(B, M, AV, T3).


%---------------------------------------
% min_index/3
% finds the index of the min value in L starting at S
% L is list
% S is starting index to look for min
% MI is the index where teh minimum value is located
%---------------------------------------
min_index(L, S, MI):-
    length(L, LL),
    LL > 1,
    SP1 is S +1,
    min_index(L, S, SP1, LL, MI).

min_index([_], _, 0).


%---------------------------------------
% min_index/5
% finds the index of the min value in L
% L is list
% CMI is current min index
% CI is current index
% EI is ending index, where to stop looking
% MI is the index where teh minimum value is located
%---------------------------------------
min_index(L, CMI, CI, EI, MI):-
    CI < EI,
    nth0(CMI, L, CMV),
    nth0(CI, L, CV),
    CIP1 is CI +1,
    (CV =< CMV
     -> min_index(L, CI, CIP1, EI, MI)
     ; min_index(L, CMI, CIP1, EI, MI)
     ).
    
min_index(_,CMI, EI, EI, CMI).


%---------------------------------------
% selection_sort/2
% sorts list L using Selection Sort algorithm
% L is unsorted list
% M is sorted list
%---------------------------------------
selection_sort(L,M):-
    length(L,LL),
    LL > 1,
    LLM1 is LL - 1,
    selection_sort(L, 0, LLM1, M).

selection_sort([], []).

selection_sort([X], [X]).


%---------------------------------------
% selection_sort/4
% sorts list L using Selection Sort algorithm
% L is unsorted list
% S is starting index
% E is ending index
% M is sorted list
%---------------------------------------
selection_sort(L, S, E, M):-
    S < E,
    min_index(L, S, MI),
    swap(L, S, MI, L1),
    SP1 is S + 1,
    selection_sort(L1, SP1, E, M).

selection_sort(L, E, E, L).





%---------------------------------------
% Merge Sort Predicates: split/3, merge/3, merge_sort/2
%----------------------------------------


%---------------------------------------
% split/3
% splits list L into 2 equal parts
% L list to be devided
% L1 is list first half of list
% L2 is second half of list
%---------------------------------------
split(L,L1,L2) :-
    append(L1, L2, L),
    length(L,LL),
    L1L is LL//2,
    length(L1, L1L).


split([L],[L],[]).


%---------------------------------------
% merge/3
% merges two already sorted list into one sorted list
% L1 first half of list (already sorted)
% L2 second half of list (already sorted)
% L3 combined list of L1 and L2 (sorted)
%---------------------------------------
merge(L1,L2,L3):-
    L1 = [L1H|L1T],
    L2 = [L2H|L2T],
    (L1H =< L2H
    ->  merge(L1T, L2, L4),
        L3 = [L1H|L4]
     ;  merge(L2T, L1, L4),
        L3 = [L2H|L4]
     ).

merge(X, [], X).
merge([], X, X).


%---------------------------------------
% merge_sort/2
% sorts list L using Merge Sort algorithm
% L is unsorted list
% M is sorted list
%---------------------------------------
merge_sort(L,M):-
    length(L,LL),
    LL > 1,
    split(L, L1, L2),
    merge_sort(L1, L1S),
    merge_sort(L2, L2S),
    merge(L1S,L2S, M).

merge_sort([],[]).

merge_sort([X],[X]).
