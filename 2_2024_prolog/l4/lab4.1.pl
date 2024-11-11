%3
choosePairs(L1,L2,Pred,Res) :-
    (var(Res) -> choosePairs1(L1,L2,Pred,Res); % если результат неизвестен, запускаем функцию 
    choosePairs1(L1,L2,Pred,Res1),
    permutation(Res1, Res)). % если известен, то находим еще раз, проверяем является ли какой-то перестановкой заданного
  
choosePairs1([], [], _, []). %случай, когда все хорошо 

choosePairs1([H1|T1], [H2|T2], Pred, Res) :- 
    (call(Pred, H1, H2) -> Res = [H1-H2|Res1], choosePairs1(T1, T2, Pred, Res1); %запускаем предикат от двух первых элементов, результат заносим, если истинен
    Res = Res1, choosePairs1(T1, T2, Pred, Res1)). % результат не изменяем, если ложен

%6
foldl(Res, _, [], Res).

foldl(Acc, Func, [H|T], Res) :-
    call(Func, Acc, H, NewAcc),  
    foldl(NewAcc, Func, T, Res).
	
addHead(_, [], []).

addHead(El, [H1|T1], [[El|H1]|T2]) :-
        addHead(El, T1, T2).

map([], _, []).
map([G1|H1], Func, [G2|H2]) :-
    T =.. [Func, G1, G2], call(T), 
    map(H1, Func, H2).
	
addHeadSyn(Lst, Elem, Rez) :- 
    addHead(Elem, Lst, Ress), append(Lst, Ress, Rez).


subsets(Lst, Subs) :- var(Lst), var(Subs), 
	writeln('Warning'), !, fail.

subsets(Lst, Subs) :-                              % если список задан
    nonvar(Lst), foldl([[]], addHeadSyn, Lst, S),  
    (var(Subs) -> 
    Subs = S; 
    map(S, msort, S1), msort(S1, S2), 
    map(Subs, msort, SSubs), msort(SSubs, SSSubs),
    S2 = SSSubs), !.
		
subsets(Lst, Subs) :-                              % если список не задан
    var(Lst), foldl([], append, Subs, R),
	sort(R, Lst), subsets(Lst, Subs).



/*
задача из лабораторной номер 2
*/

numLst(N, Lst) :-
    (nonvar(N) ->
    numLst0(N, Lst));
    ((var(N), var(Lst)) -> numLst1(N, Lst, 0)); % ?N ?Lst
    (var(N) -> list2num(Lst, 0, N)). % ?N +Lst

% пункт а
% Базовый случай: если число 0, то список его цифр это [0]
numLst0(0, [0]) :- !.

numLst0(N, Lst) :-
    N > 0,
    numlst_helper(N, [], Lst).

numlst_helper(0, Acc, Acc) :- !.  
numlst_helper(N, Acc, Lst) :-
    N > 0,
    LastDigit is N mod 10, 
    Remaining is N // 10,   
    numlst_helper(Remaining, [LastDigit|Acc], Lst).

% пункт б

% ?N +Lst
% в изначальный предикат добавили условие var(N)
list2num([], Acc, Acc).
list2num([D | T], Acc, N) :-
    NewAcc is Acc * 10 + D,
    list2num(T, NewAcc, N).

% ?N ?Lst
numLst1(N, Lst, P) :- 
    N = P,                 % сопоставляем первое число
    numLst0(P, Lst);      % запускаем вариант +N ?Lst
    P1 is P + 1,           % увеличиваем P
    numLst1(N, Lst, P1).