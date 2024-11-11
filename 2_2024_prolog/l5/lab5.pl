%1




%2
visit(yes).
visit(no).

hlp(X,Y) :- 
	call(X) -> call(Y); 
	true.
	
attendclass :- 
	visit(Andrey), visit(Dmitriy), visit(Boris), visit(Victor), visit(Grigoriy),
	hlp((Andrey = yes, Dmitriy = yes),(Boris = no)),
	hlp((Andrey = yes, Dmitriy = no),(Boris = yes, Victor = no)),
	(Andrey = yes, Victor = yes; Andrey = no, Victor = no),
	hlp((Dmitriy = yes),(Grigoriy = no)),
	hlp((Boris = no, Victor = no),(Dmitriy = yes)),
	hlp((Boris = no, Victor = yes), (Dmitriy = no, Grigoriy = yes)),
	hlp((Boris = yes),(Andrey = yes)),
	
	write(andrey), write(':'), write(Andrey), nl,
	write(dmitriy), write(':'), write(Dmitriy), nl,
	write(boris), write(':'), write(Boris), nl,
	write(victor), write(':'), write(Victor), nl,
	write(grigoriy), write(':'), write(Grigoriy), nl.

 
%4

solution_tier_2 :-


%Барон Мюнхгаузен говорит правду только один день в неделю. Какой это день, если
%известно следующее:
%1. Однажды он сказал: «Я лгу по понедельникам и вторникам»;
%2. На следующий день он сказал: «Сегодня — или четверг, или суббота, или воскресенье»;
%3. Ещё на следующий день он сказал: «Я лгу по средам и пятницам».
%nth(?integer, ?list, ?term)

next(monday,tuesday).
next(tuesday,wednesday).    
next(wednesday,thursday).
next(thursday,friday).
next(friday,saturday).
next(saturday,sunday).
next(sunday,monday).

days(Days) :- Days = [monday, tuesday, wednesday, thursday, friday, saturday, sunday].

% получаем тройки дней
% X - день, когда он говорит правду 
threeDays(Today, Tomorrow, Aftertomorrow) :- 
    days(Days), length(Days, L),
	member(X, Days), nth(C, Days, X), 
	Today = X, 
    C1 is C + 1, 
    C2 is C + 2,

	(C1 =< L -> nth(C1, Days, Tomorrow); 
	C11 is C1 - L, 
	nth(C11, Days, Tomorrow)),

    (C2 =< L -> nth(C2, Days, Aftertomorrow);
	C22 is C2 - L,
	nth(C22, Days, Aftertomorrow)).

solution([X,T,A]) :- %поиск правдивой тройки дней
	threeDays(X, T, A),
    (((X \= monday, X \= tuesday)                   %1 
	-> 
    (T \= thursday, T \= saturday, T \= sunday),    %2
	(A = wednesday; A = friday))                    %3
    ;
	((T = thursday; T = saturday; T = sunday)       %2
	-> 
	(X = monday; X = tuesday),                      %1
	(A = wednesday; A = friday))                    %3
	;
    ((A \= wednesday, A \= friday)                  %3 
	-> 
	(X = monday; X = tuesday),                      %1
	(T \= thursday, T \= saturday, T \= sunday)     %2
	);
    (X = monday, X = tuesday, T \= thursday, T \= saturday, T \= sunday, A = wednesday; A = friday)).

cheker([X,Y,Z]) :-
    




 









%3
% Определяем высказывания каждого мальчика
statement(misha, \+ broke(misha), \+ broke(kolya)).   % Миша: "Я не разбивал окно, и Коля тоже."    1 был
statement(kolya, \+ broke(misha), broke(sergey)).    % Коля: "Миша не разбивал окно, это Сергей разбил!" 1 был
statement(sergey, \+ broke(sergey), broke(misha)).    % Сергей: "Я не делал этого, стекло разбил Миша."

boy(misha).
boy(kolya).
boy(sergey).


boys(X) :- permutation([misha, kolya, sergey], X).

broke(yes).
broke(no).

solve0 :-
    broke(Misha), broke(Kolya), broke(Sergey),
    (\+ broke(Misha), \+ broke(Kolya)) -> 
        (((\+ broke(Misha), \+ broke(Sergey)); (broke(Misha), broke(Sergey))),
        ((broke(Sergey), \+ broke(Misha)))); 
        (((\+ broke(Sergey), \+ broke(Misha)); (broke(Sergey), broke(Misha))), (broke(Misha), \+ broke(Sergey)));

    (\+ broke(Misha), broke(Sergey)) ->
        (((\+ broke(Misha), broke(Kolya)); (broke(Misha), \+ broke(Kolya))),
        ((broke(Sergey), \+ broke(Misha)))); 
        (((\+ broke(Sergey), \+ broke(Misha)); (broke(Sergey), broke(Misha))), (broke(Misha), broke(Kolya)));

    (\+ broke(Sergey), broke(Misha)) ->
        (((\+ broke(Misha), broke(Kolya)); (
            broke(Misha), \+ broke(Kolya))),
        ((broke(Misha), \+ broke(Sergey))));
        (((broke(Misha), broke(Sergey)); (\+ broke(Misha), \+ broke(Sergey))), (broke(Misha), broke(Kolya))),
        
    write(misha), write(' '), write(Misha), nl,
    write(kolya), write(' '), write(Kolya), nl,
    write(sergey), write(' '), write(Sergey), nl.