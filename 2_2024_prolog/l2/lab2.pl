/*
task1//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
*/

myReverse(List, Reversed) :-
    myReverseAcc(List, [], Reversed).

myReverseAcc([], Acc, Acc).

% берем голову списка и добавляем её в переменную, в которой копим, продолжая обрабатывать хвост
myReverseAcc([Head|Tail], Accumulator, Reversed) :-
    myReverseAcc(Tail, [Head|Accumulator], Reversed).

% предикат для проверки, что список – палиндром
palindrome(List) :-
    maplist(lower, List, LowerList),    % приводим символы к нижнему регистру
    myReverse(LowerList, LowerList).       % проверяем, что список равен обратному

% Преобразуем символ к нижнему регистру
lower('A', 'a').
lower('B', 'b').
lower('C', 'c').
lower(X, X) :- member(X, ['a', 'b', 'c']).

/*
task2//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
*/
numLst(N, Lst) :-
    (integer(N) ->
    numLst(N, [], Lst));
    ((var(N), var(Lst)) -> numLst1(N, Lst, 0)); % ?N ?Lst
    (var(N) -> list2num(Lst, 0, N)). % ?N +Lst

% пункт а
numLst(N, Acc, Lst) :- 
    N < 10 -> Lst = [N | Acc];  % если число меньше 10, заносим в результат 
    N1 is N div 10, % отсавшееся число, с которым продолжим работать в следующем шаге
    N2 is N mod 10, % отстаток от деления - последняя цифра, заносим в аккумулятор 
    numlst(N1, [N2 | Acc], Lst).

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
    numLst(P, [], Lst);      % запускаем вариант +N ?Lst
    P1 is P + 1,           % увеличиваем P
    numLst1(N, Lst, P1).


/*
task3//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
*/
 

% пункт а

hasDigitA(Num, Digit) :-
    Num < 10,             % если число меньше 10 и состоит из цифры
    Num = Digit.

hasDigitA(Num, Digit) :-
    Num >= 10,                 % если число больше 9
    (Digit is Num mod 10;      % либо последняя цифра числа искомая, либо ищем дальше
    Num1 is Num div 10,
    hasDigitA(Num1,Digit)).

hasDigit(Num, Digit) :-         % модуль, чтобы не работать с отрицательным
    Num1 is abs(Num),
    hasDigitA(Num1,Digit).


% пункт б
qntDigit(From, To, _, 0) :-
    From > To, !.                   

qntDigit(From, To, Digit, Count) :-
    From =< To,                      % продолжаем работать пока не прошли все числа из диапазона
    From1 is From + 1,               % переходим к новому диапазону, увеличивая первое число
    (hasDigit(From, Digit) -> qntDigit(From1, To, Digit, Count1),            % если число имеет в составе нужную цифру, идем в случай, где увеличиваем счетчик
    Count is Count1 + 1;                                                    
    qntDigit(From1, To, Digit, Count)).    % если не имеет, то счетчик не увеличиваем


/*
task4//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
*/
% пункт а
% insert(+Tree, +Integer, ?NewTree)
insert(nil, Integer, tr(Integer, nil, nil)).  % вставляем в пустое дерево

insert(tr(Integer, Left, Right), Integer, tr(Integer, Left, Right)).  % значение уже есть

insert(tr(Root, Left, Right), Integer, tr(Root, NewLeft, Right)) :-
    Integer < Root,                         % вставляем в левое поддерево
    insert(Left, Integer, NewLeft).

insert(tr(Root, Left, Right), Integer, tr(Root, Left, NewRight)) :-
    Integer > Root,                         % вставляем в правое поддерево
    insert(Right, Integer, NewRight).

% пункт б

% contains(+Tree, ?Integer)
contains(tr(Integer, _, _), Integer).  % элемент найден

contains(tr(Root, Left, _), Integer) :-
    (var(Integer) -> contains(Left, Integer)); %если переменная, то перебор по всем поддеревьям
    Integer < Root,                  % ищем в левом поддереве
    contains(Left, Integer).

contains(tr(Root, _, Right), Integer) :-
    (var(Integer) -> contains(Right, Integer));
    Integer >= Root,                  % ищем в правом поддереве
    contains(Right, Integer).


% пункт в
isSearchTreeRight(nil, _).

isSearchTreeRight(tr(Root, Left, Right), Integer) :- 
    Root > Integer,                          
    isSearchTreeRight(Right, Root),          
    isSearchTreeLeft(Left, Integer).
                    
isSearchTreeLeft(nil, _).

isSearchTreeLeft(tr(Root, Left, Right), Integer) :- 
    Root < Integer, 
    isSearchTreeLeft(Left, Root), 
    isSearchTreeRight(Right, Root).

isSearchTree(tr(Root, Left, Right)) :- 
    isSearchTreeRight(Right, Root), 
    isSearchTreeLeft(Left, Root).

% пункт г "временно отсутствует"



