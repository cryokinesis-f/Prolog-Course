/*
task1
*/

% X - основание, N - показатель степени, Pow - результат возведения X в степень N.
powFast(_, 0, 1). 

powFast(X, N, Pow) :-
    N > 0,
    N mod 2 =:= 0, 
    N1 is N // 2, 
    powFast(X, N1, Pow1), 
    Pow is Pow1 * Pow1.  

powFast(X, N, Pow) :-
    N > 0,
    N mod 2 =:= 1, 
    N1 is N // 2,
    powFast(X, N1, Pow1),
    Pow is X * Pow1 * Pow1. 

/*
powFast(X, N, Pow) :-
    N > 0,
    N1 is N // 2,
    powFast(X, N1, Pow1),
    Pow2 is Pow1*Pow1,
    (N mod 2 =:= 1 -> Pow is X * Pow2 ; Pow is Pow2). 
*/


/*
task2
*/
even(0).
even(X) :- 0 =:= X mod 2.
odd(1).
odd(X) :- 1 =:= X mod 2.
/*
filter1([], _, []).

filter1([H|T], Pred, [H|Res]) :-
    call(Pred, H),   
    filter1(T, Pred, Res), !.

filter1([H|T], Pred, Res) :-
    \+ call(Pred, H), 
    filter1(T, Pred, Res), !.
*/
filter([], _, []).

filter([H|T], Pred, Res) :-
    filter(T, Pred, Res1),
    (call(Pred, H) -> Res = [H|Res1] ; Res = Res1).

/*
task3
*/

choosePairs(L1,L2,Pred,Res,FRes) :-
    (var(Res) ->
    choosePairs1(L1,L2,Pred,Res);
    choosePairs1(L1,L2,Pred,Res); 
    Res1 = permutation(Res),
    choosePairs2(L1,L2,Pred,Res1,FRes)).

choosePairs1([], [], _, []).

choosePairs1([H1|T1], [H2|T2], Pred, Res) :-
    (call(Pred, H1, H2) -> Res = [H1-H2|Res1], choosePairs1(T1, T2, Pred, Res1);
    Res = Res1, choosePairs1(T1, T2, Pred, Res1)).

choosePairs2([], [], _, Res,Res) :- !, fail.

choosePairs2((L1,L2,Pred,Res1,FRes)) :-
    choosePairs1(L1,L2,Pred,Res1);
    Res2 = permutation(Res1),
    choosePairs2((L1,L2,Pred,Res2,FRes)).




/* 
task4
*/
add(A,B,C) :- C is A+B.
addu(A,B,C) :- C is A * B.

foldl(Res, _, [], Res).

% применяем предикат Func к текущему элементу и продолжаем вычисление для хвоста списка
foldl(Acc, Func, [H|T], Res) :-
    call(Func, Acc, H, NewAcc),  % вызываем предикат с текущим аккумулятором и элементом H
    foldl(NewAcc, Func, T, Res).  % рекурсивный вызов для хвоста списка

/*
task5
*/

addHead(_, [], []).

% добавляем элемент в голову каждого списка из List1 и получаем List2
addHead(El, [H1|T1], [[El|H1]|T2]) :-
    addHead(El, T1, T2).


%6

map([], _, []).
map([G1|H1], Func, [G2|H2]) :-
    T =.. [Func, G1, G2], call(T), 
    map(H1, Func, H2).
	
addHeadSyn(Lst, Elem, Rez) :- addHead(Elem, Lst, Ress), append(Lst, Ress, Rez).

subsets(Lst, Subs) :- var(Lst), var(Subs), 
	writeln('Warning'), !, fail.

subsets(Lst, Subs) :-  nonvar(Lst), foldl([[]], addHeadSyn, Lst, S),  
    (
		var(Subs) -> 
        Subs = S 
		; 
        map(S, msort, S1), msort(S1, S2), 
        map(Subs, msort, SSubs), msort(SSubs, SSSubs),
        S2 = SSSubs
	), 
	!.
		
subsets(Lst, Subs) :- 
    var(Lst), foldl([], append, Subs, R),
	sort(R, Lst), subsets(Lst, Subs).

