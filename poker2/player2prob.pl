play([],0).
play([[Hand1,Hand2]|Rst], Num_Wins) :-
winner(Hand1, Hand2, Winner),
(Winner = Hand1, play(Rst,Remaining), Num_Wins is 1 + Remaining ;
play(Rst, Num_Wins)).

check_draw(H1, H2, R) :-
H1 = [[A,_],[B,_],[C,_],[D,_],[E,_]],
H2 = [[F,_],[G,_],[H,_],[I,_],[J,_]],
(   A = F, B = G, C = H, D = I, E = J
->  R = 1
; R = 0).
    
winner(H1, H2, Winner) :-
H1 = H2,
Winner is 0;
sort_hand(H1, Sorted_Hand1),
sort_hand(H2, Sorted_Hand2),
check_draw(Sorted_Hand1, Sorted_Hand2, R1),
( R1 = 1
->  Winner = 0
;   
    (determine_hand(Sorted_Hand1,  X1),
    determine_hand(Sorted_Hand2,  X2),
    beats(X1, X2, Verdict),
    (Verdict = X1, Winner is 1;
    Verdict = X2, Winner is 0;
    Verdict = tie, tiebreak(X1, Sorted_Hand1, Sorted_Hand2, SortedWinner),
    (SortedWinner = left, Winner is 1 ;
    SortedWinner = right, Winner is 0)))
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Tiebreaks
tiebreak(straight_flush, H1, H2, Winner)  :- higher_last_card(H1, H2, Winner).
tiebreak(four_of_a_kind, H1, H2, Winner)  :- higher_middle_card(H1, H2, Winner).
tiebreak(full_house, H1, H2, Winner)      :- higher_middle_card(H1, H2, Winner).
tiebreak(flush, H1, H2, Winner)           :- tiebreak(high_card, H1, H2, Winner).
tiebreak(straight, H1, H2, Winner)        :- higher_last_card(H1, H2, Winner).
tiebreak(three_of_a_kind, H1, H2, Winner) :- higher_middle_card(H1, H2, Winner).

tiebreak(two_pair, H1, H2, Winner) :-
isolate_pairs(H1, [HighCard1,_], [LowCard1,_], Last1),
isolate_pairs(H2, [HighCard2,_], [LowCard2,_], Last2),
(beats_with_hand(H1, HighCard1, H2, HighCard2, Winner),
Winner \= tie;
beats_with_hand(H1, LowCard1, H2, LowCard2, Winner),
Winner \= tie;
beats_with_hand(H1, Last1, H2, Last2, Winner)).

tiebreak(pair, H1, H2, Winner) :-
isolate_pair(H1, [PairCard1,_], Rst1),
isolate_pair(H2, [PairCard2,_], Rst2),
(beats_with_hand(H1, PairCard1, H2, PairCard2, Winner), Winner \= tie ;
tiebreak(high_card, Rst1, Rst2, Winner)).

tiebreak(high_card, H1, H2, X) :- 
reverse(H1, RevH1),
reverse(H2, RevH2),
highest_card_chain(RevH1, RevH2, X).

beats_with_hand(H1, C1, H2, C2, X) :-
beats(C1, C2, C1), X = left ;
beats(C1, C2, C2), X = right ;
X = tie.

% Really ugly.  How to better do this?
isolate_pairs(Hand, High_Pair, Low_Pair, Last) :-
[[V1,S1],[V2,S2],[V3,S3],[V4,S4],[V5,S5]] = Hand,
(V5 = V4, High_Pair = [[V4,S4],[V5,S5]],
(V3 = V2, Low_Pair = [[V3,S3],[V2,S2]], Last = [V1,S1] ;
V1 = V2, Low_Pair = [[V1,S1],[V2,S2]], Last = [V3,S3])) ;
(Low_Pair = [[V1,S1],[V2,S2]], 
High_Pair = [[V3,S3],[V4,S4]],
Last = [V5,S5]).

isolate_pair(Hand, Pair, Rst) :-
[[V1,S1],[V2,S2],[V3,S3],[V4,S4],[V5,S5]] = Hand,
(V1 = V2, Pair = [[V1,S1],[V2,S2]], Rst = [[V3,S3],[V4,S4],[V5,S5]] ;
V2 = V3, Pair = [[V3,S3],[V2,S2]], Rst = [[V1,S1],[V4,S4],[V5,S5]] ;
V4 = V3, Pair = [[V3,S3],[V4,S4]], Rst = [[V1,S1],[V2,S2],[V5,S5]] ;
V4 = V5, Pair = [[V5,S5],[V4,S4]], Rst = [[V1,S1],[V2,S2],[V3,S3]]).

highest_card_chain([H1|T1], [H2|T2], X) :-
beats(H1,H2,Verdict),
(Verdict = H1, X = left ;
Verdict = H2, X = right ;
Verdict = tie, highest_card_chain(T1,T2,X)).

higher_last_card(H1,H2,Winner) :-
H1 = [_,_,_,_,[V1,_]],
H2 = [_,_,_,_,[V2,_]],
beats(V1,V2,Higher),
(Higher = V1, Winner = left ;
Higher = V2, Winner = right).

higher_middle_card(H1, H2, Winner) :-
H1 = [_,_,[V1,_],_,_],
H2 = [_,_,[V2,_],_,_],
beats(V1,V2,Higher),
(Higher = V1, Winner = left;
Higher = V2, Winner = right).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Hand determination
determine_hand([[10,X],[jack,X],[queen,X],[king,X],[ace,X]], royal_flush).

determine_hand([[A,X],[B,X],[C,X],[D,X],[E,X]], straight_flush) :-
successor(E,D), successor(D,C), successor(C,B), successor(B,A).

determine_hand([[C,_],[A,_],[A,_],[A,_],[B,_]], four_of_a_kind) :-
C = A ; B = A.

determine_hand([[A,_],[B,_],[C,_],[D,_],[E,_]], full_house) :-
A = B, D = E, (C = D ; C = B).

determine_hand([[_,X],[_,X],[_,X],[_,X],[_,X]], flush).

determine_hand([[A,_],[B,_],[C,_],[D,_],[E,_]], straight) :-
successor(E,D), successor(D,C), successor(C,B), successor(B,A).

determine_hand([[A,_],[B,_],[C,_],[D,_],[E,_]], three_of_a_kind) :-
(A = B, B = C); (B = C, C = D); (C = D, D = E).

determine_hand([[A,_],[A,_],[B,_],[B,_],[_,_]], two_pair).
determine_hand([[_,_],[A,_],[A,_],[B,_],[B,_]], two_pair).
determine_hand([[A,_],[A,_],[_,_],[B,_],[B,_]], two_pair).

determine_hand([[A,_],[B,_],[C,_],[D,_],[E,_]], pair) :-
A = B; B = C; C = D; D = E.

determine_hand(_,high_card).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Hand sorting (for easier pattern matching).
sort_hand([], []).
sort_hand([H|T], Sorted) :-
filter_by_high_card(H,T,Lower,Higher),
sort_hand(Lower,SortedLower),
sort_hand(Higher,SortedHigher),
append(SortedLower, [H|SortedHigher], Sorted).


filter_by_high_card(_, [], [], []).  
filter_by_high_card(Pivot, [H|T], [H|Lower], Higher) :-
beats(Pivot,H,Z),
(Z = Pivot ; Z = tie),
filter_by_high_card(Pivot, T, Lower, Higher).
filter_by_high_card(Pivot, [H|T], Lower, [H|Higher]) :-
beats(Pivot,H,H),
filter_by_high_card(Pivot, T, Lower, Higher).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Card and Hand Precedence
beats([V,_],[V,_],tie).
beats([V1,S],[V2,_],[V1,S]) :- value_greater_than(V1,V2).
beats([V1,_],[V2,S],[V2,S]) :- value_greater_than(V2,V1).

beats(X,X,tie).
beats(X,Y,X) :- value_greater_than(X,Y).
beats(X,Y,Y) :- value_greater_than(Y,X).

successor(royal_flush, straight_flush).   successor(straigh_flush, four_of_a_kind).
successor(four_of_a_kind, full_house).    successor(full_house, flush).
successor(flush, straight).               successor(straight, three_of_a_kind).
successor(three_of_a_kind, two_pair).     successor(two_pair, pair).
successor(pair, high_card).

successor(ace,king).     successor(king,queen).   successor(queen,jack).
successor(jack,10).      successor(10,9).         successor(9,8).
successor(8,7).          successor(7,6).          successor(6,5).
successor(5,4).          successor(4,3).          successor(3,2).

value_greater_than(X,Y) :-
successor(X,P),
(Y = P;
value_greater_than(P,Y)).

%%%%%%%%%%%%%%%%%%%%% calculate probability %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate_hand_type([],S,0,0,0,0,0,0,0,0,0).

calculate_hand_type([P|Q], S, R1, R2, R3, R4, R5, R6, R7, R8, R9) :-
append(P, S, NS),
conversion(NS, NSV1),
classify_temp(NSV1, R),
%print(NSV1), print('    '), print(R), nl,
calculate_hand_type(Q,S,T1,T2,T3,T4,T5,T6,T7,T8,T9),
%print(T1),print(T2),print(T3),print(T4),print(T5),print(T6),print(T7),print(T8),print(T9),nl,
(   R =:= 1
->  R1 is T1 + 1, R2 = T2, R3 = T3, R4 = T4, R5 = T5, R6 = T6, R7 = T7, R8 = T8, R9 = T9
;   (   R =:= 2
    ->  R2 is T2 + 1, R1 = T1, R3 = T3, R4 = T4, R5 = T5, R6 = T6, R7 = T7, R8 = T8, R9 = T9
    ;   (   R =:= 3
        ->  R3 is T3 + 1, R2 = T2, R1 = T1, R4 = T4, R5 = T5, R6 = T6, R7 = T7, R8 = T8, R9 = T9
        ;   (   R =:= 4
            ->  R4 is T4 + 1, R2 = T2, R3 = T3, R1 = T1, R5 = T5, R6 = T6, R7 = T7, R8 = T8, R9 = T9
            ;   (   R =:= 5
                ->  R5 is T5 + 1, R2 = T2, R3 = T3, R4 = T4, R1 = T1, R6 = T6, R7 = T7, R8 = T8, R9 = T9
                ;   (   R =:= 6
                    ->  R6 is T6 + 1, R2 = T2, R3 = T3, R4 = T4, R5 = T5, R1 = T1, R7 = T7, R8 = T8, R9 = T9
                    ;   (   R =:= 7
                        ->  R7 is T7 + 1, R2 = T2, R3 = T3, R4 = T4, R5 = T5, R6 = T6, R1 = T1, R8 = T8, R9 = T9
                        ;   (   R =:= 8
                            ->  R8 is T8 + 1, R2 = T2, R3 = T3, R4 = T4, R5 = T5, R6 = T6, R7 = T7, R1 = T1, R9 = T9
                            ;   R9 is T9 + 1, R2 = T2, R3 = T3, R4 = T4, R5 = T5, R6 = T6, R7 = T7, R8 = T8, R1 = T1
                            )
                        )
                    )
                )
            )
        ) 
    )
).


calculate_prob([], S1, S2, 0).
calculate_prob([P|Q], S1, S2, R1) :-
append(P, S2, NS2),
winner(S1,NS2,X1),
%print(S1), print(' ------ '), print(NS2), print(' -- '), print(X1), nl,
calculate_prob(Q,S1,S2,T1),
R1 is T1 + X1.

calculate_probV2([], S1, S2, 0).
calculate_probV2([P|Q], S1, S2, R1) :-
append([P], S2, NS2),
winner(S1,NS2,X1),
%print(S1), print(' ------ '), print(NS2), print(' -- '), print(X1), nl,
calculate_probV2(Q,S1,S2,T1),
R1 is T1 + X1.

%%%%%%%%%%%%%%%%%%%%% get all combinations of given length %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

combination(0, _, []) :- !.
combination(N, L, [V|R]) :-
N > 0,
NN is N - 1,
unknown(V, L, Rem),
combination(NN, Rem, R).

unknown(X,[X|L],L).
unknown(X,[_|L],R) :-
unknown(X,L,R).

combination_all(N, L, Bag) :-
findall(R, combination(N, L, R), Bag), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% cross product %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

product(A,B,C) :- findall([X,Y],(member(X,A),member(Y,B)),C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% merge two list %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge_list([],L,L).
merge_list([H|T],L,[H|M]):-
merge_list(T,L,M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% remove nth element and return remaining list %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

removeNthElem(0, [Nth|X], Pre, L2, Nth) :-
merge_list(Pre, X, L2), !.

removeNthElem(N, [E|X], Pre, L2, Nth) :-
N > 0,
N1 is N-1,
append(Pre, [E], Z),
removeNthElem(N1, X, Z, L2, Nth).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% conversion %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert([2,X], card(2,X)).
convert([3,X], card(3,X)).
convert([4,X], card(4,X)).
convert([5,X], card(5,X)).
convert([6,X], card(6,X)).
convert([7,X], card(7,X)).
convert([8,X], card(8,X)).
convert([9,X], card(9,X)).
convert([10,X], card(10,X)).
convert([jack,X], card(11,X)).
convert([queen,X], card(12,X)).
convert([king,X], card(13,X)).
convert([ace,X], card(14,X)).

conversion([],[]).
conversion([H|T],[H1|T1]) :-
convert(H,H1),
conversion(T,T1). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% rank calculation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suit(hearts).
suit(clubs).
suit(spades).
suit(diamonds).

card(I, J) :-
    integer(I),
    suit(J).

flush(X) :-
    nth1(1, X, A),
    nth1(2, X, B),
    nth1(3, X, C),
    nth1(4, X, D),
    nth1(5, X, E),

    A = card(_F, Z),
    B = card(_G, Z),
    C = card(_H, Z),
    D = card(_I, Z),
    E = card(_J, Z).


straight(X) :-
    nth1(1, X, A),
    nth1(2, X, B),
    nth1(3, X, C),
    nth1(4, X, D),
    nth1(5, X, E),

    A = card(F, _K),
    B = card(G, _L),
    C = card(H, _M),
    D = card(I, _N),
    E = card(J, _O),
    Values = [F, G, H, I, J],
    sort(Values, Sorted),
    nth1(1, Sorted, A1),
    nth1(2, Sorted, B1),
    nth1(3, Sorted, C1),
    nth1(4, Sorted, D1),
    nth1(5, Sorted, E1),
    E1 - D1 =:= 1,
    D1 - C1 =:= 1,
    C1 - B1 =:= 1,
    B1 - A1 =:= 1.
    

straightflush(X) :-
    straight(X),
    flush(X).

comparecards(N, A, B, C, D, E) :-

    N =:= 4,
    format('One Pair ~n');

    N =:= 3,
    A =:= B,
    B =:= C,
    format('Three of a Kind ~n');

    N =:= 3,    
    B =:= C,
    C =:= D,
    format('Three of a Kind ~n');

    N =:= 3,
    C =:= D,
    D =:= E,
    format('Three of a Kind ~n');

    N =:= 3,
    format('Two Pair ~n');

    N =:= 2,
    A =:= B,
    B =:= C,
    D =:= E,
    format('Full House ~n');

    N =:= 2,
    A =:= B,
    C =:= D,
    D =:= E,
    format('Full House ~n');

    N =:= 2,
    format('Four of a Kind ~n');

    format('High Card ~n').

prep(X) :-
    nth1(1, X, A),
    nth1(2, X, B),
    nth1(3, X, C),
    nth1(4, X, D),
    nth1(5, X, E),

    A = card(F, _R),
    B = card(G, _S),
    C = card(H, _T),
    D = card(I, _U),
    E = card(J, _V),

    Y = [F, G, H, I, J],

    sort(Y, Sorted),
    msort(Y, SortedM),
    length(Sorted, Q),
    nth1(1, SortedM, K),
    nth1(2, SortedM, L),
    nth1(3, SortedM, M),
    nth1(4, SortedM, N),
    nth1(5, SortedM, P),

    comparecards(Q, K, L, M, N, P).
    
classify(X) :-
    straightflush(X),
    format('Straight Flush ~n');
    flush(X),
    format('Flush ~n');
    straight(X),
    format('Straight ~n');
    prep(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Rank V2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% One Pair - 1
% Three of a Kind - 2
% Two Pair - 3
% Full House - 4
% Four of a Kind - 5
% High Card - 6

comparecards_temp(N, A, B, C, D, E, Result) :-

    N =:= 4,
    Result = 1;

    N =:= 3,
    A =:= B,
    B =:= C,
    Result = 2;

    N =:= 3,    
    B =:= C,
    C =:= D,
    Result = 2;

    N =:= 3,
    C =:= D,
    D =:= E,
    Result = 2;

    N =:= 3,
    Result = 3;

    N =:= 2,
    A =:= B,
    B =:= C,
    D =:= E,
    Result = 4;

    N =:= 2,
    A =:= B,
    C =:= D,
    D =:= E,
    Result = 4;

    N =:= 2,
    Result = 5;

    Result = 6.

prep_temp(X, Res) :-
    nth1(1, X, A),
    nth1(2, X, B),
    nth1(3, X, C),
    nth1(4, X, D),
    nth1(5, X, E),

    A = card(F, _R),
    B = card(G, _S),
    C = card(H, _T),
    D = card(I, _U),
    E = card(J, _V),

    Y = [F, G, H, I, J],

    sort(Y, Sorted),
    msort(Y, SortedM),
    length(Sorted, Q),
    nth1(1, SortedM, K),
    nth1(2, SortedM, L),
    nth1(3, SortedM, M),
    nth1(4, SortedM, N),
    nth1(5, SortedM, P),

    comparecards_temp(Q, K, L, M, N, P, Res).
   
% Straight Flush - 7
% Flsuh - 8
% Straight - 9

classify_temp(X, Result) :-
    straightflush(X),
    Result = 7;
    flush(X),
    Result = 8;
    straight(X),
    Result = 9;
    prep_temp(X, Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% decide winner %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

callme(B1, B2, A1, A2) :-
S = [hearts, clubs, spades, diamonds],
In = [2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king, ace],
Blind is 1,
product(In, S, Pack),
random(0, 52, Pl11),
removeNthElem(Pl11, Pack, [], R1, P1C1),
random(0, 51, Pl21),
removeNthElem(Pl21, R1, [], R2, P2C1),
random(0, 50, Pl12),
removeNthElem(Pl12, R2, [], R3, P1C2),
random(0, 49, Pl22),
removeNthElem(Pl22, R3, [], R4, P2C2),
random(0, 48, Pl13),
removeNthElem(Pl13, R4, [], R5, P1C3),
random(0, 47, Pl23),
removeNthElem(Pl23, R5, [], R6, P2C3),
random(0, 46, Pl14),
removeNthElem(Pl14, R6, [], R7, P1C4),
random(0, 45, Pl24),
removeNthElem(Pl24, R7, [], R8, P2C4),
random(0, 44, Pl15),
removeNthElem(Pl15, R8, [], R9, P1C5),
random(0, 43, Pl25),
removeNthElem(Pl25, R9, [], R10, P2C5),

merge_list([P2C4], R10, Rem2Temp),
merge_list([P2C5], Rem2Temp, Rem2),
merge_list([P1C4], R10, Rem1Temp),
merge_list([P1C5], Rem1Temp, Rem1),

print('Player 1 cards are -> '), print(P1C1), print(' , '), print(P1C2), print(' , '), print(P1C3), nl,
print('Player 1 Hidden cards are -> '), print(P1C4), print(' , '), print(P1C5), nl, 
print('Player 2 cards are -> '), print(P2C1), print(' , '), print(P2C2), print(' , '), print(P2C3), nl,
print('Player 2 Hidden cards are -> '), print(P2C4), print(' , '), print(P2C5), nl, nl,

combination_all(2, Rem2, Combo2),
calculate_prob(Combo2, [P1C1,P1C2,P1C3,P1C4,P1C5], [P2C1,P2C2,P2C3], Ct1),
Pr1 is Ct1/946,
print('Probability of Player 1 winning is -> '), print(Pr1), nl,
    
combination_all(2, Rem1, Combo1),    
calculate_prob(Combo1, [P2C1,P2C2,P2C3,P2C4,P2C5], [P1C1,P1C2,P1C3], Ct2),
Pr2 is Ct2/946,
print('Probability of Player 2 winning is -> '), print(Pr2), nl, nl,

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% One Pair - 1
% Three of a Kind - 2
% Two Pair - 3
% Full House - 4
% Four of a Kind - 5
% High Card - 6
% Straight Flush - 7
% Flush - 8
% Straight - 9

print('--------------------------------------------------------------------------------------------------'), nl, nl,

calculate_hand_type(Combo2, [P2C1,P2C2,P2C3],P1,P2,P3,P4,P5,P6,P7,P8,P9),
V1 is P1/946, V2 is P2/946, V3 is P3/946, V4 is P4/946, V5 is P5/946,
V6 is P6/946, V7 is P7/946, V8 is P8/946, V9 is P9/946,
print('Probabilities for Player2 as judged by Player1'), nl,
print('One Pair -> '), print(V1), nl,
print('Three of a Kind -> '), print(V2), nl,
print('Two Pair -> '), print(V3), nl,
print('Full House -> '), print(V4), nl,
print('Four of a Kind -> '), print(V5), nl,
print('High Card -> '), print(V6), nl,
print('Straight Flush -> '), print(V7), nl,
print('Flush -> '), print(V8), nl,
print('Straight -> '), print(V9), nl, nl,

calculate_hand_type(Combo1, [P1C1,P1C2,P1C3],Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9),
W1 is Q1/946, W2 is Q2/946, W3 is Q3/946, W4 is Q4/946, W5 is Q5/946,
W6 is Q6/946, W7 is Q7/946, W8 is Q8/946, W9 is Q9/946,
print('Probabilities for Player1 as judged by Player2'), nl,
print('One Pair -> '), print(W1), nl,
print('Three of a Kind -> '), print(W2), nl,
print('Two Pair -> '), print(W3), nl,
print('Full House -> '), print(W4), nl,
print('Four of a Kind -> '), print(W5), nl,
print('High Card -> '), print(W6), nl,
print('Straight Flush -> '), print(W7), nl,
print('Flush -> '), print(W8), nl,
print('Straight -> '), print(W9), nl, nl, nl,


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print('------------------------------------------- ROUND ONE ---------------------------------------------------'), nl, nl,

print('Player 1, enter bet '), nl,
read(Bet1),
print('Player 2, do you match it '), nl,
read(Bet2),

winner([P1C1,P1C2,P1C3,P1C4,P1C5], [P2C1,P2C2,P2C3,P2C4,P2C5], Winner1), !,
winner([P2C1,P2C2,P2C3,P2C4,P2C5], [P1C1,P1C2,P1C3,P1C4,P1C5], Winner2), !,

( Bet1 =:= Bet2
->
print('Player1 4th card is -> '), print(P1C4), nl,
print('Player2 4th card is -> '), print(P2C4), nl, nl,
merge_list([P2C5], R8, Rem2V2),
merge_list([P1C5], R8, Rem1V2),

calculate_probV2(Rem2V2, [P1C1,P1C2,P1C3,P1C4,P1C5], [P2C1,P2C2,P2C3,P2C4], Ct3),
Pr3 is Ct3/43,
print('Probability of Player 1 winning is -> '), print(Pr3), nl,
   
calculate_probV2(Rem1V2, [P2C1,P2C2,P2C3,P2C4,P2C5], [P1C1,P1C2,P1C3,P1C4], Ct4),
Pr4 is Ct4/43,
print('Probability of Player 2 winning is -> '), print(Pr4), nl, nl,


print('------------------------------------------- ROUND TWO ---------------------------------------------------'), nl, nl,

print('Player 1, enter bet '), nl,
read(Bet3),
print('Player 2, do you match it '), nl,
read(Bet4), nl,

( Bet3 =:= Bet4

->  print('Player1 5th card is -> '), print(P1C5), nl, print('Player2 5th card is -> '), print(P2C5), nl,
    (   Winner1 = 1
    ->  print('Player1 wins. '), nl, A1 is B1 + Blind + Bet1 + Bet3, A2 is B2 - Blind - Bet1 - Bet3
    ;   ( Winner2 = 1
        ->  print('Player 2 wins. '), nl, A2 is B2 + Blind + Bet2 + Bet4, A1 is B1 - Blind - Bet1 - Bet3
        ;   print('Its a draw. '), nl, A1 = B1, A2 = B2
        )
    )
;   (   Bet4 =:= 0
    ->  print('Player1 wins. '), nl, A1 is B1 + Blind + Bet1, A2 is B2 - Blind - Bet1
    ;   print('Player1 do you match it. '), nl, read(Bet5), nl,
        (   Bet5 =:= 0
        ->  print('Player2 wins'), nl, A1 is B1 - Blind - Bet1 - Bet3, A2 is B2 + Blind + Bet2 + Bet3
        ;   (   Winner1 = 1
            ->  print('Player1 wins. '), nl, A1 is B1 + Blind + Bet1 + Bet4, A2 is B2 - Blind - Bet1 - Bet4
            ;   ( Winner2 = 1
                ->  print('Player 2 wins. '), nl, A2 is B2 + Blind + Bet2 + Bet4, A1 is B1 - Blind - Bet1 - Bet4
                ;   print('Its a draw. '), nl, A1 = B1, A2 = B2
                )
            )
        )   
    )
)
; print('Player1 wins. '), A1 is B1 + Blind, A2 is B2 - Blind

).

callme_automate(F, B1, B2, A1, A2) :-
S = [hearts, clubs, spades, diamonds],
In = [2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king, ace],
Blind is 1,
product(In, S, Pack),
random(0, 52, Pl11),
removeNthElem(Pl11, Pack, [], R1, P1C1),
random(0, 51, Pl21),
removeNthElem(Pl21, R1, [], R2, P2C1),
random(0, 50, Pl12),
removeNthElem(Pl12, R2, [], R3, P1C2),
random(0, 49, Pl22),
removeNthElem(Pl22, R3, [], R4, P2C2),
random(0, 48, Pl13),
removeNthElem(Pl13, R4, [], R5, P1C3),
random(0, 47, Pl23),
removeNthElem(Pl23, R5, [], R6, P2C3),
random(0, 46, Pl14),
removeNthElem(Pl14, R6, [], R7, P1C4),
random(0, 45, Pl24),
removeNthElem(Pl24, R7, [], R8, P2C4),
random(0, 44, Pl15),
removeNthElem(Pl15, R8, [], R9, P1C5),
random(0, 43, Pl25),
removeNthElem(Pl25, R9, [], R10, P2C5),

merge_list([P2C4], R10, Rem2Temp),
merge_list([P2C5], Rem2Temp, Rem2),
merge_list([P1C4], R10, Rem1Temp),
merge_list([P1C5], Rem1Temp, Rem1),
    
print('Player 1 cards are -> '), print(P1C1), print(' , '), print(P1C2), print(' , '), print(P1C3), nl,
print('Player 1 Hidden cards are -> '), print(P1C4), print(' , '), print(P1C5), nl, 
print('Player 2 cards are -> '), print(P2C1), print(' , '), print(P2C2), print(' , '), print(P2C3), nl,
print('Player 2 Hidden cards are -> '), print(P2C4), print(' , '), print(P2C5), nl, nl,


% convert to integer
conversion([P1C1,P1C2,P1C3,P1C4,P1C5], P1S),
print('Classification for Player1 -> '), classify(P1S),
conversion([P2C1,P2C2,P2C3,P2C4,P2C5], P2S),
print('Classification for Player2 -> '), classify(P2S),

writeln(F, ['Player 1 Cards -> ', P1C1,P1C2,P1C3,'Hidden Cards -> ', P1C4,P1C5]),
writeln(F, ['Player 2 Cards -> ', P2C1,P2C2,P2C3,'Hidden Cards -> ', P2C4,P2C5]),

combination_all(2, Rem2, Combo2),
calculate_prob(Combo2, [P1C1,P1C2,P1C3,P1C4,P1C5], [P2C1,P2C2,P2C3], Ct1),
Pr1 is Ct1/946,

atom_concat('Probability of Player 1 winning is -> ', Pr1, MSG3),
writeln(F, MSG3),
    
combination_all(2, Rem1, Combo1),    
calculate_prob(Combo1, [P2C1,P2C2,P2C3,P2C4,P2C5], [P1C1,P1C2,P1C3], Ct2),
Pr2 is Ct2/946,

atom_concat('Probability of Player 2 winning is -> ', Pr2, MSG4),
writeln(F, MSG4),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% One Pair - 1
% Three of a Kind - 2
% Two Pair - 3
% Full House - 4
% Four of a Kind - 5
% High Card - 6
% Straight Flush - 7
% Flush - 8
% Straight - 9

writeln(F, '--------------------------------------------------------------------------------------------------'),
writeln(F, ' '),

calculate_hand_type(Combo2, [P2C1,P2C2,P2C3],P1,P2,P3,P4,P5,P6,P7,P8,P9),
V1 is P1/946, V2 is P2/946, V3 is P3/946, V4 is P4/946, V5 is P5/946,
V6 is P6/946, V7 is P7/946, V8 is P8/946, V9 is P9/946,
writeln(F, 'Probabilities for Player2 as judged by Player1'),
write(F, 'One Pair -> '), writeln(F, V1),
write(F, 'Three of a Kind -> '), writeln(F, V2),
write(F, 'Two Pair -> '), writeln(F, V3),
write(F, 'Full House -> '), writeln(F, V4),
write(F, 'Four of a Kind -> '), writeln(F, V5),
write(F, 'High Card -> '), writeln(F, V6),
write(F, 'Straight Flush -> '), writeln(F, V7),
write(F, 'Flush -> '), writeln(F, V8),
write(F, 'Straight -> '), writeln(F, V9),
writeln(F, ' '),
writeln(F, ' '),

calculate_hand_type(Combo1, [P1C1,P1C2,P1C3],Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9),
W1 is Q1/946, W2 is Q2/946, W3 is Q3/946, W4 is Q4/946, W5 is Q5/946,
W6 is Q6/946, W7 is Q7/946, W8 is Q8/946, W9 is Q9/946,
write(F, 'Probabilities for Player1 as judged by Player2'),
write(F, 'One Pair -> '), writeln(F, W1),
write(F, 'Three of a Kind -> '), writeln(F, W2),
write(F, 'Two Pair -> '), writeln(F, W3),
write(F, 'Full House -> '), writeln(F, W4),
write(F, 'Four of a Kind -> '), writeln(F, W5),
write(F, 'High Card -> '), writeln(F, W6),
write(F, 'Straight Flush -> '), writeln(F, W7),
write(F, 'Flush -> '), writeln(F, W8),
write(F, 'Straight -> '), writeln(F, W9),

writeln(F, '--------------------------------------------------------------------------------------------------'),
writeln(F, ' '),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% bet proportional to probability

Bet1 = 2, Bet2 = 5,
winner([P1C1,P1C2,P1C3,P1C4,P1C5], [P2C1,P2C2,P2C3,P2C4,P2C5], Winner1), !,
winner([P2C1,P2C2,P2C3,P2C4,P2C5], [P1C1,P1C2,P1C3,P1C4,P1C5], Winner2), !,

(   Pr1 > 0.5
->  I1 = 1
;   I1 = 0
),

(   Pr2 > 0.5
->  I2 = 1
;   I2 = 0
),

( I1 = 1, I2 = 1
->
merge_list([P2C5], R8, Rem2V2),
merge_list([P1C5], R8, Rem1V2),

calculate_probV2(Rem2V2, [P1C1,P1C2,P1C3,P1C4,P1C5], [P2C1,P2C2,P2C3,P2C4], Ct3),
Pr3 is Ct3/43,

calculate_probV2(Rem1V2, [P2C1,P2C2,P2C3,P2C4,P2C5], [P1C1,P1C2,P1C3,P1C4], Ct4),
Pr4 is Ct4/43,

atom_concat('Probability of Player 1 winning is -> ', Pr3, MSG5),
writeln(F, MSG5),
atom_concat('Probability of Player 2 winning is -> ', Pr4, MSG6),
writeln(F, MSG6),

(   Pr3 > 0.7
->  I3 = 1
;   I3 = 0
),

(   Pr4 > 0.7
->  I4 = 1
;   I4 = 0
),


( I3 =:= 1, I4 =:= 1

->  (   Winner1 =:= 1
    ->  writeln(F, 'Player1 wins. '), A1 is B1 + Blind + Bet1 + Bet2, A2 is B2 - Blind - Bet1 - Bet2
    ;   ( Winner2 =:= 1
        ->  writeln(F, 'Player 2 wins. '), A2 is B2 + Blind + Bet1 + Bet2, A1 is B1 - Blind - Bet1 - Bet2
        ;   writeln(F, 'Its a draw. '), A1 = B1, A2 = B2
        )
    )
;   (   I3 =:= 0, I4 =:= 1
    ->  writeln(F, 'Player1 quits. Player2 wins. '), A2 is B2 + Blind + Bet1 + Bet2, A1 is B1 - Blind - Bet1 - Bet2
    ;   (   I3 =:= 1, I4 =:= 0
        ->  writeln(F, 'Player2 quits. Player1 wins. '), A2 is B2 - Blind - Bet1 - Bet2, A1 is B1 + Blind + Bet1 + Bet2
        ;   writeln(F, 'Its a draw. '), A1 = B1, A2 = B2
        )  
    )
)
;
(   I1 =:= 0, I2 =:= 1
->  writeln(F, 'Player1 quits. Player2 wins. '), A1 is B1 - Blind - Bet1, A2 is B2 + Blind + Bet1
;   (   I1 =:= 1, I2 =:= 0
    ->  writeln(F, 'Player2 quits. Player1 wins. '), A2 is B2 - Blind - Bet1, A1 is B1 + Blind + Bet1
    ;   writeln(F, 'Its a draw. '), A1 = B1, A2 = B2
    )
)
).

poker(S1, S2) :-
(   S1 > 0, S2 > 0
->
callme(S1, S2, A1, A2),
print('Player1 -> '), print(A1), nl, 
print('Player2 -> '), print(A2), nl, nl,
print('################################################### NEW GAME #######################################################'), nl, nl,
poker(A1, A2)
;   true
).

poker_automate(F, S1, S2) :-
(   S1 > 0, S2 > 0
->
callme_automate(F, S1, S2, A1, A2),
print('Player1 -> '), print(A1), nl,
print('Player2 -> '), print(A2), nl, nl,
atom_concat('Player1 -> ', A1, MSG7),
atom_concat('Player2 -> ', A2, MSG8),
writeln(F, MSG7),
writeln(F, MSG8),
writeln(F, ' '),
print('#################################################### NEW GAME ###########################################################'), nl, nl,
writeln(F, '#################################################### NEW GAME ###########################################################'), writeln(F, ' '),
poker_automate(F, A1, A2)
;   true
).

poker_automate(F) :-
print('First Bet = 2. Second Bet = 5'), nl,
open(F, append, Y),
poker_automate(Y, 10, 10),
close(Y).