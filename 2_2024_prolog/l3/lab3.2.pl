:- initialization(consult('kinship.pl')).


% а 
pred(X, Y) :- 
    parent(X, Y).              % X родитель Y

pred(X,Y) :-
    setof(Z, (parent(X, Z), pred(Z, Y), X \= Y), Z).


% б
%%%%%
full_sibling(X, Y) :-
    parent(Father, X), parent(Father, Y), male(Father),  % Общий отец
    parent(Mother, X), parent(Mother, Y), female(Mother),% Общая мать
    X \= Y.                                              % Разные личности
%%%%%

brother(X, Y) :- 
    full_sibling(X, Y),
    male(X), 
    X \= Y.

% в

married(X, Y) :-
    setof(C, (parent(X, C), parent(Y, C), X \= Y), _).

husband(X, Y) :- 
    married(X, Y), 
    male(X).


% г
sister(X, Y) :- 
    full_sibling(X, Y),
    female(X), 
    X \= Y.

cousin(X, Y) :- 
    parent(P1, X), 
    parent(P2, Y), 
    brother(P1, P2).

cousin(X, Y) :- 
    parent(P1, X), 
    parent(P2, Y), 
    sister(P1, P2).

% д

nephews(Nephews, Person) :-
    setof(Nephew, (full_sibling(Person, Sibling), parent(Sibling, Nephew)), Nephews).

% е

family([Father, Mother | Children]) :-
    married(Father, Mother),
    findall(Child, (parent(Father, Child), parent(Mother, Child)), ChildrenList),
    sort(ChildrenList, Children).  