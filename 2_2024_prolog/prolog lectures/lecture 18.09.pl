% вычисояющие предикаты   consult('lecture 18.09.pl').
inc(N,Res) :- Res is N + 1.
sqr(X,Res) :- Res is X*X.

% map(Lst1, Func, Lst2).
% Lst = (cons Head Tail).
% HeadRes = f Head
% TailRes = (map f Tail)
% LstRes = (cons HeadRes TailRes)
% Base: (map () _) = ()
map([], _, []).
map([H1|T1], Func, Lst2) :- 
    T =.. [Func, H1, H2], call(T),
    map(T1, Func, T2),
    Lst2 = [H2 | T2].

map1([], _, []).
map1([H1|T1], Func, [H2|T2]) :-
    T =.. [Func, H1, H2], call(T),
    map1(T1, Func, T2).
/*
| ?- map([1,2,3,4], sqr, LRes).  

LRes = [1,4,9,16]

yes
*/

allCompute(N1,N2,Res) :-
   member(Op, [+, -, *, div, mod, /]),
   T =.. [Op, N1, N2],
   Res is T.



allCompute1(N1,N2,Res) :- Res is N1+N2.
allCompute1(N1,N2,Res) :- Res is N1-N2.
allCompute1(N1,N2,Res) :- Res is N1*N2.

/*
| ?- allCompute1(4,5,Res).

Res = 9 ? ;

Res = -1 ? ;

Res = 20

yes
| ?- allCompute(4,5,Res). 

Res = 9 ? ;

Res = -1 ? ;

Res = 20 ? 
*/

allComputeOp(N1,N2,Res,Op) :-
    member(Op, [+, -, *, div, mod, /]),
    T =.. [Op, N1, N2],
    Res is T.


%предикат myMax(X,Y,Z) = Z - максимум из X Y
myMax0(X,Y,Z) :- X > Y, Z = X.
myMax0(X,Y,Z) :- X =< Y, Z = Y.


% A или В -> C <----> A -> C, B ->C

myMax1(X,Y,Z) :-
    X > Y, Z = X ; X =< Y, Z = Y.

myMax2(X,Y,Z) :- X > Y, Z = X.
myMax2(_,Y,Z) :- Z = Y.
%неправда

/* ! - отсечение - предикат, который всегда истинен. Отсечение говорит "Пролог, забудь все точки отката, 
которые были созданы при доказательстве этого предиката, или вызванне из него" */
myMax3(X,Y,Z) :- X > Y, !, Z = X.
myMax3(_,Y,Z) :- Z = Y.

/* отсечение надо ставить после условия ветвления*/

myMax4(X,Y,Z) :- X > Y, !, Z = X ; Z = Y.

/* вариант с дизъюнкцией - ветвление в полный рост
! - then-блок //отсекает точки отката на данном уровне и ниже
; - else-блок
-> - аналог !, -> отсекает точки отката только на уровне данного предиката
fail - тождественно ложен
*/

myMax4(X,Y,Z) :- X > Y, -> Z = X ; Z = Y.

/*
правильный вариант доказательства ложности, перед fail стоит отсечение

*/

%наивная проверка на простоту 
prime(N) :- N = 1, !, fail.
prime(N) :- N mod 2 =:= 0, !, fail.
% большой тяжелый точный алгоритм.
 