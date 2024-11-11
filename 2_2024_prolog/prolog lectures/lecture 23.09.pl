% mouse, cat, dog, donkey, horse, elephant
biggerThan(cat,mouse).
biggerThan(cat,zemerojka).
biggerThan(dog,cat).
biggerThan(donkey,dog).
biggerThan(horse,donkey).
biggerThan(elephant,horse).

bigger(A1,A2) :- 
    biggerThan(A1,A2) ;
    biggerThan(A1,A), bigger(A,A2).
/*
findall/3(X, переменная, значения которой хотим собрать
trem, запрос, истинность которого хотим установить
Lst, список всех ответов в том порядке, в котором пролог их находит)
*/
/*
findall(X, bigger(X,dog), Lst).
Lst = [donkey,horse,elephant]

| ?- findall(X, (bigger(horse,X),bigger(X,dog)), Lst).

Lst = [donkey]

| ?- findall(X-Y, bigger(X,Y),Lst).

Lst = [cat-mouse,dog-cat,donkey-dog,horse-donkey,elephant-horse,dog-mouse,donkey-cat,donkey-mouse,horse-dog,horse-cat,horse-mouse,elephant-donkey,elephant-dog,elephant-cat,elephant-mouse]

yes



findall, sort === setof
| ?- setof(X, bigger(Y,X),Lst).                

Lst = [mouse]
Y = cat ? ;

Lst = [cat,mouse]
Y = dog ? ;

Lst = [cat,dog,mouse]
Y = donkey ? ;

Lst = [cat,dog,donkey,horse,mouse]
Y = elephant ? ;

Lst = [cat,dog,donkey,mouse]
Y = horse

| ?- setof(X, Y^bigger(Y,X), Lst). 

Lst = [cat,dog,donkey,horse,mouse]
*/

biggerAndSmaller(X) :-
    bigger(X,horse), bigger(dog,X).

/*

findall: если в запросе есть переменная,
которая не идёт в ответ, то собираются все
ответы по всем значениям этой переменной

setof: если в запросе есть переменная, 
которая не идёт в ответ, то ответы выдаются по каждому
возможному значению этой переменной

findall: пустой список, если ответов нет

setof: если ответов нет, ложен
*/

/*

setof(<object>, <peremennaya>^zapros, Lst) 

| ?- setof(X, Y^Z^(bigger(Y,X),bigger(X,Z)),Lst)
.

Lst = [cat,dog,donkey,horse]

yes

*/

