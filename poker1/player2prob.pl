play([],0).
play([[Hand1,Hand2]|Rst], Num_Wins) :-
winner(Hand1, Hand2, Winner),
(Winner = Hand1, play(Rst,Remaining), Num_Wins is 1 + Remaining ;
play(Rst, Num_Wins)).


check_draw(H1, H2, R) :-
H1 = [[A,_],[B,_],[C,_],[D,_],[E,_]],
H2 = [[F,_],[G,_],[H,_],[I,_],[J,_]],
(   A = F, B = G, C = H, D = I, E = J
->	R = 1
;	R = 0).
    
winner(H1, H2, Winner) :-
H1 = H2,
Winner is 0;
sort_hand(H1, Sorted_Hand1),
sort_hand(H2, Sorted_Hand2),
check_draw(Sorted_Hand1, Sorted_Hand2, R1),
(	R1 = 1
->  Winner = 0
;   
    (determine_hand(Sorted_Hand1,  X1),
    determine_hand(Sorted_Hand2,  X2),
    beats(X1, X2, Verdict),
    (Verdict = X1, Winner is 1;
    Verdict = X2, Winner is 0;
    Verdict = tie, tiebreak(X1, Sorted_Hand1, Sorted_Hand2, SortedWinner),
    (SortedWinner = left, Winner is 1 ;
    SortedWinner = right, Winner is 0 )))
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
Verdict = tie, X = none).

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

%%%%%%%%%%%%%%%%%%%%% Calculate %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_it_raw(P1, Q1, R1) :-
nth0(0,Q1,S0),
nth0(1,Q1,S1),
nth0(2,Q1,S2),
nth0(3,Q1,S3),
nth0(4,Q1,S4),
nth0(5,Q1,S5),
nth0(6,Q1,S6),
nth0(7,Q1,S7),
nth0(8,Q1,S8),
nth0(9,Q1,S9),
nth0(10,Q1,S10),
nth0(11,Q1,S11),
nth0(12,Q1,S12),
nth0(13,Q1,S13),
nth0(14,Q1,S14),
nth0(15,Q1,S15),
nth0(16,Q1,S16),
nth0(17,Q1,S17),
nth0(18,Q1,S18),
nth0(19,Q1,S19),
nth0(20,Q1,S20),

( winner(P1,S0,B0), B0 = 0
-> R1 = 0
; ( winner(P1,S1,B1), B1 = 0
  -> R1 = 0
  ; ( winner(P1,S2,B2), B2 = 0
  	-> R1 = 0
    ; ( winner(P1,S3,B3), B3 = 0
  	  -> R1 = 0
      ; ( winner(P1,S4,B4), B4 = 0
  		-> R1 = 0
  		; ( winner(P1,S5,B5), B5 = 0
  		  -> R1 = 0
  		  ; ( winner(P1,S6,B6), B6 = 0
  			-> R1 = 0
  			; ( winner(P1,S7,B7), B7 = 0
  			  -> R1 = 0
  			  ; ( winner(P1,S8,B8), B8 = 0
  				-> R1 = 0
  				; ( winner(P1,S9,B9), B9 = 0
  				  -> R1 = 0
  				  ;	( winner(P1,S10,B10), B10 = 0
  					-> R1 = 0
  					; ( winner(P1,S11,B11), B11 = 0
  					  -> R1 = 0
  					  ; ( winner(P1,S12,B12), B12 = 0
  						-> R1 = 0
  						; ( winner(P1,S13,B13), B13 = 0
  						  -> R1 = 0
  						  ; ( winner(P1,S14,B14), B14 = 0
  							-> R1 = 0
  							; ( winner(P1,S15,B15), B15 = 0
  							  -> R1 = 0
   							  ; ( winner(P1,S16,B16), B16 = 0
  								-> R1 = 0
  								; ( winner(P1,S17,B17), B17 = 0
  								  -> R1 = 0
  								  ; ( winner(P1,S18,B18), B18 = 0
  									-> R1 = 0
  									; ( winner(P1,S19,B19), B19 = 0
  									  -> R1 = 0
  									  ; ( winner(P1,S20,B20), B20 = 0
  										-> R1 = 0
  										;  R1 = 1
  										)
  									  ) 
  									) 
  								  )
  								) 
  							  ) 
  							) 
  						  ) 
  						)
  					  )
  					)
  				  )
  				)
  			  )
  			)
  		  )
  		)
	  )
    )
  )
).

compare_them_all(P, Q, R) :-
nth0(0,P,S0),
nth0(1,P,S1),
nth0(2,P,S2),
nth0(3,P,S3),
nth0(4,P,S4),
nth0(5,P,S5),
nth0(6,P,S6),
nth0(7,P,S7),
nth0(8,P,S8),
nth0(9,P,S9),
nth0(10,P,S10),
nth0(11,P,S11),
nth0(12,P,S12),
nth0(13,P,S13),
nth0(14,P,S14),
nth0(15,P,S15),
nth0(16,P,S16),
nth0(17,P,S17),
nth0(18,P,S18),
nth0(19,P,S19),
nth0(20,P,S20),

compare_it_raw(S0,Q,A0),
compare_it_raw(S1,Q,A1),
compare_it_raw(S2,Q,A2),
compare_it_raw(S3,Q,A3),
compare_it_raw(S4,Q,A4),
compare_it_raw(S5,Q,A5),
compare_it_raw(S6,Q,A6),
compare_it_raw(S7,Q,A7),
compare_it_raw(S8,Q,A8),
compare_it_raw(S9,Q,A9),
compare_it_raw(S10,Q,A10),
compare_it_raw(S11,Q,A11),
compare_it_raw(S12,Q,A12),
compare_it_raw(S13,Q,A13),
compare_it_raw(S14,Q,A14),
compare_it_raw(S15,Q,A15),
compare_it_raw(S16,Q,A16),
compare_it_raw(S17,Q,A17),
compare_it_raw(S18,Q,A18),
compare_it_raw(S19,Q,A19),
compare_it_raw(S20,Q,A20),
X is A0+A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20,
(	X > 0
->	R = 1
;	R = 0
).

solve_it(S1, S2, R) :-
compare_them_all(S1, S2, R).

%%%%%%%%%%%%%%%%%%%%% calculate probability %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate_prob([], S1, S2, 0).
calculate_prob([P|Q], S1, S2, R1) :-
append(P, S1, NS1),
append(P, S2, NS2),
combination_all(5, NS1, S1All),
combination_all(5, NS2, S2All),
solve_it(S1All, S2All, X1),
% write(NS1), write('  '), write(NS2), write('  '), write(X1), nl,
calculate_prob(Q,S1,S2,T1),
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

%%% remove nth element and return remaining list %%%

removeNthElem(0, [Nth|X], Pre, L2, Nth) :-
merge_list(Pre, X, L2), !.

removeNthElem(N, [E|X], Pre, L2, Nth) :-
N > 0,
N1 is N-1,
append(Pre, [E], Z),
removeNthElem(N1, X, Z, L2, Nth).

callme :-
S = [hearts, clubs, spades, diamonds],
In = [2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king, ace],
Blind is 10,
product(In, S, Pack),
random(0, 52, Pl11),
removeNthElem(Pl11, Pack, [], R1, P1C1),
random(0, 51, Pl21),
removeNthElem(Pl21, R1, [], R2, P2C1),
random(0, 50, Pl12),
removeNthElem(Pl12, R2, [], R3, P1C2),
random(0, 49, Pl22),
removeNthElem(Pl22, R3, [], R4, P2C2),
random(0, 48, G1),
removeNthElem(G1, R4, [], R5, GC1),
random(0, 47, G2),
removeNthElem(G2, R5, [], R6, GC2),
random(0, 46, G3),
removeNthElem(G3, R6, [], R7, GC3),
random(0, 45, G4),
removeNthElem(G4, R7, [], R8, GC4),
random(0, 44, G5),
removeNthElem(G5, R8, [], _, GC5),
print('Player 1 cards are -> '), print(P1C1), print(' , '), print(P1C2), nl,
print('Player 2 cards are -> '), print(P2C1), print(' , '), print(P2C2), nl,

print('Player 1, enter bet '), nl,
read(B1),
print('Player 2, do you match it '), nl,
read(B2),


( B1 =:= B2
->  
print('The 3 cards are -> '), print(GC1), print(GC2), print(GC3), nl,
combination_all(2, R6, Combo),
calculate_prob(Combo, [P1C1,P1C2,GC1,GC2,GC3], [P2C1,P2C2,GC1,GC2,GC3], Ct1),
calculate_prob(Combo, [P2C1,P2C2,GC1,GC2,GC3], [P1C1,P1C2,GC1,GC2,GC3], Ct2),
write(Ct1), write('   '), write(Ct2), nl,
Total is 23*47,
Pr1 is Ct1 / Total,
Pr2 is Ct2 / Total,
Pr3 is 1-Pr1-Pr2,
write('Probability for Player 1 ->  '), write(Pr1), nl, 
write('Probability for Player 2 ->  '), write(Pr2), nl,
write('Draw Probability ->  '), write(Pr3), nl,

print('Player 1, enter bet '), nl,
read(B3),
print('Player 2, do you match it '), nl,
read(B4),

( B3 =:= B4
->  
print('The 4th card is -> '), print(GC4), nl,
print('Player 1, enter bet '), nl,
read(B5),
print('Player 2, do you match it '), nl,
read(B6),

( B5 =:= B6
->  
print('The 5th card is -> '), print(GC5), nl,
print('Player 1, enter bet '), nl,
read(B7),
print('Player 2, do you match it '), nl,
read(B8),

%%---------------------------------------------------
%% number the clssify results. sort it. take first
%%---------------------------------------------------

( B7 =:= B8
->  
combination_all(5, [P1C1,P1C2,GC1,GC2,GC3,GC4,GC5], P1All),
combination_all(5, [P2C1,P2C2,GC1,GC2,GC3,GC4,GC5], P2All),
solve_it(P1All, P2All, Re1),
solve_it(P2All, P1All, Re2),
AMT is Blind + B1 + B3 + B5 + B7,

( Re1 > 0, Re2 = 0 
->  print('Player 1 won -> '), print(AMT), nl
;
( Re2 > 0, Re1 = 0
-> print('Player 2 won -> '), print(AMT), nl
;  print('Its a tie'), nl)
)
; AMT is Blind + B1 + B2 + B3 + B4 + B5 + B6, print('Player 2 Loses'), print('Player 1 wins -> '), print(AMT), nl
)
; AMT is Blind + B1 + B2 + B3 + B4, print('Player 2 loses'), print('Player 1 wins -> '), print(AMT), nl
)
; AMT is Blind + B1 + B2, print('Player 2 loses'), print('Player 1 wins -> '), print(AMT), nl
)
; print('Player 2 loses'), print('Player 1 wins -> '), print(Blind), nl
).

