/*
task3%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Напишите предикат, принимающий истинные значения на чётных целых
числах. 
Операция взятия остатка от деления X mod Y, проверка равенства
двух чисел — операция =:=, неравенства — операция =\=.
*/

eval_mod(0).
%eval_mod(N) :- N = 0. %, !.
%eval_mod(N) :- N = 1, !, fail.
eval(N) :- N1 is N mod 2, eval_mod(N1).

chetnoe(N) :- N mod 2 =:= 0.

/*
task4%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

bin(N,K,A,B) :- N is K - 2, !, A = B.
bin(First,K,Prom,Second) :-
    First >= K,
    Prom1 is Prom * K,
    K1 is K + 2,
    bin(First,K1,Prom1,Second).

task4(A,B) :-
    (0 =:= A mod 2), !,
    bin(A,2,1,B).

task4(A,B) :-
    bin(A,1,1,B).

/*
task5%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*
prime(K) :- 
    K > 1,
    prime(K,2).

prime(I,D) :- D * D > I, !.

prime(I,D) :-
%    I >= D * D,
    I mod D > 0,
    D1 is D + 1,
    prime(I,D1).
*/
/*
prime(K) :- K > 2, K mod 2 =:= 0, !, false.
*/

prime(2).
prime(K) :-
    K > 2,  K mod 2 =\= 0,
    prime(K,3).

prime(I,D) :- D * D > I, !.
prime(I,D) :-
    I mod D > 0,
    D1 is D + 2,
    prime(I,D1).


/*
task6%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/
sirakuz1(N,_,TekN) :- N < TekN, !.
sirakuz1(N,A0,TekN) :-
%    N >= TekN,
    write(A0),
    nl,
    ((0 =:= A0 mod 2) -> A1 is A0 div 2; A1 is (3 * A0 + 1)),
    TekN1 = TekN + 1,
    sirakuz1(N,A1,TekN1).

sirakuz(N,A0) :- sirakuz1(N,A0,0).
/*
task7%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
*/
nextMonth(jan,feb).
nextMonth(feb,mar).
nextMonth(mar,apr).
nextMonth(apr,may).
nextMonth(may,jun).
nextMonth(jun,jul).
nextMonth(jul,aug).
nextMonth(aug,sep).
nextMonth(sep,oct).
nextMonth(oct,nov).
nextMonth(nov,dec).
nextMonth(dec,lan).

numOfDays(jan,31).
numOfDays(feb,28).
numOfDays(mar,31).
numOfDays(apr,30).
numOfDays(may,31).
numOfDays(jun,30).
numOfDays(jul,31).
numOfDays(aug,31).
numOfDays(sep,30).
numOfDays(oct,31).
numOfDays(nov,30).
numOfDays(dec,31).

nextDate(date(M1,D1),date(M2,D2)) :-
    numOfDays(M1,Da1),
    D1 >= 1, D1 =< Da1,
    ( D1 = Da1, nextMonth(M1,M2), D2 = 1 ; D1 \= Da1, M2 = M1, D2 is D1 + 1 ).
