# Отчет по курсовому проекту
## по курсу "Логическое программирование"

### студент: Глушин Никита Дмитриевич 

## Результат проверки

Вариант задания:

 - [ ] стандартный, без NLP (на 3)
 - [x] стандартный, с NLP (на 3-4)
 - [ ] продвинутый (на 3-5)
 
| Преподаватель     | Дата         |  Оценка       |
|-------------------|--------------|---------------|
| Сошников Д.В. |              |               |
| Левинская М.А.|              |               |

> *Комментарии проверяющих (обратите внимание, что более подробные комментарии возможны непосредственно в репозитории по тексту программы)*

## Введение

В ходе выполнения данного курсового проекта я получу понимание того, что такое логическое программирование. Изучу и реализую простейший NLP для естественно-языкового интерфейса. В качестве приятного бонуса узнаю больше о родословной европейской знати.

## Задание

- Получить родословную европейской знати, преобразовать данные в предикаты языка Prolog.
- Реализовать предикаты различных родственников, в том числе продиктованных вариантом задания (свекровь, невестка). 
- Создать предикат для определения степени родства двух людей. На его основе вывести предикаты для поиска родственников и определения их количества.

## Получение родословного дерева

Я получил родословное дерево, скачав его по [ссылке](http://www.rusgenealog.ru/gedcom/royal_gen.zip). Общее количество индивидуумов в нем - 24901.

## Конвертация родословного дерева

Для ускорения работы по конвертации родословного дерева я решил использовать Python.

Первоначально отфильтруем данные по нужным нам словам.
```python
words = ['NAME', 'SEX', 'FAMS', 'FAMC', 'MARNM']
with open(sys.argv[1], 'r', encoding='cp1251') as file:
    lines = [line.strip('\n') for line in file.readlines() if line.split()[1] in words]
    data = [i.replace('\iiin', '').split(' ')[1:] for i in lines]

for i in range(len(data)):
    if data[i][0] == '_MARNM' and '?' not in data[i][1] and '//' not in data[i][1]:
        data[i - 1][2] = data[i][1]
    data[i] = ' '.join(data[i]).replace('/', '').split(' ')
    if data[i][0] == 'NAME':
        data[i][1] = ' '.join(data[i][1:]).strip().replace("'", '')
        data[i] = data[i][:2]
data = [e for e in data if e[0] in words[:-1]]
```

Соберем словарь семей. В значении ячейки словаря лежат два множества. В первом - родители, во втором - дети.
```python
families = dict()
person = [str(), str()]
for elem in data:
    if elem[0] in words[:2]:
        person[elem[0] == 'SEX'] = elem[1]
    if elem[0] == 'FAMS':
        if elem[1] not in families:
            families[elem[1]] = [set(), set()]
        families[elem[1]][0].add(tuple(person))
    elif elem[0] == 'FAMC':
        if elem[1] not in families:
            families[elem[1]] = [set(), set()]
        families[elem[1]][1].add(tuple(person))
```

Соберем все воедино и запишем в выходной файл
```python
with open('tree.pl', 'w') as file:
    res = []
    for family in families:
        for parent in families[family][0]:
            for child in families[family][1]:
                if parent[1] == 'M':
                    res.append(f"father('{parent[0]}', '{child[0]}').")
                else:
                    res.append(f"mother('{parent[0]}', '{child[0]}').")
    res.sort(key=lambda x: x[0]) # сортируем, чтобы сначала шли все отцы, потом матери
    for i in res:
        print(i, file=file)
```

Получившийся результат:
```prolog
father('Рюрик I, кн. Новгородский', 'Олег Младший, кн. Ладожский').
father('Рюрик I, кн. Новгородский', 'Игорь I Старый, вел. кн. Киевский').
father('Игорь I Старый, вел. кн. Киевский', 'Святослав I, вел.кн. Киевский').
father('Святослав I, вел.кн. Киевский', 'Олег Святославич, кн. Древлянский').
father('Святослав I, вел.кн. Киевский', 'Ярополк I Святославич, вел.кн. Киевский').
father('Святослав I, вел.кн. Киевский', 'Владимир I Святославич Святой, вел.кн. Киевский').
...
```

## Предикат поиска родственника

Для того, чтобы находить определенных родственников, я реализовал для них соответствующие предикаты, основанные на `father` и `mother`.

```prolog
child(Child, Parent) :- father(Parent, Child); mother(Parent, Child).
parent(Parent, Child) :- child(Child, Parent).

grandChild(Child, GrandParent) :- child(Child, Parent), child(Parent, GrandParent).
grandParent(Parent, Child) :- grandChild(Child, Parent).
grandFather(Parent, Child) :- grandParent(Parent, Child), male(Parent).
grandMother(Parent, Child) :- grandParent(Parent, Child), female(Parent).

sibling(P1, P2) :- child(P1, X), child(P2, X), P1 \== P2.
brother(Child, Brother) :- sibling(Child, Brother), male(Brother).
sister(Child, Sister) :- sibling(Child, Sister), female(Sister).

uncle(Child, Uncle) :- child(Child, Parent), brother(Parent, Uncle).
aunt(Child, Aunt) :- child(Child, Parent), sister(Parent, Aunt).
nephew(Child, Person) :- uncle(Child, Person); aunt(Child, Person).

cousine(Child1, Child2) :- child(Child1, Parent1), child(Child2, Parent2), sibling(Parent1, Parent2).

married(Wife, Husband) :- child(Child, Husband), child(Child, Wife), female(Wife), male(Husband).

motherInLaw(MotherInLaw, DaughterInLaw) :- married(DaughterInLaw, Husband), mother(MotherInLaw, Husband).
daughterInLaw(DaughterInLaw, MotherInLaw) :- motherInLaw(MotherInLaw, DaughterInLaw).
``` 

Примеры работы некоторых предикатов:

```shell
?- uncle(Uncle, 'Олег Младший, кн. Ладожский').
Uncle = 'Святослав I, вел.кн. Киевский' .

?- married(Wife, 'Рюрик I, кн. Новгородский').
Wife = 'Ефанда' .

?- motherInLaw(MotherInLaw, 'Малуша').
MotherInLaw = 'Ольга Мудрая, кн. Киевская' .

?- daughterInLaw(DaughterInLaw, 'Ольга Мудрая, кн. Киевская')
DaughterInLaw = 'Малуша' .
```

## Определение степени родства

Воспользуемся упомянутыми выше предикатами. Дополнительно реализуем предикат для поиска в ширину. Он понадобится для определения дальних родственников.

```prolog
bfs([[Finish|T]|_], Finish, [Finish|T], _).
bfs([P|Q], Finish, Path, D) :-
    findall(NP, prolong(P, NP), NPs),
    append(Q, NPs, Prolonged),
    D1 is D - 1,
    D1 > 0, !,
    bfs(Prolonged, Finish, Path, D1).
bfs([_|Q], Finish, Path, D) :- bfs(Q, Finish, Path, D).

bfsStart(Start, Finish, Path, D) :-
    bfs([[Start]], Finish, Path, D).
```

Пусть тогда `relationship(Degree, Person1, Person2)` будет определять степень родства `Degree` между `Person1` и `Person2`.
Пердикат будет иметь вид:

```prolog
relationship(Degree, P1, P2) :-
    child(P1, P2),
    Degree = 'child'.

relationship(Degree, P1, P2) :-
    parent(P1, P2),
    Degree = 'parent'.

relationship(Degree, P1, P2) :-
    father(P1, P2),
    Degree = 'father'.

relationship(Degree, P1, P2) :-
    mother(P2, P1),
    Degree = 'mother'.

relationship(Degree, P1, P2) :-
    grandChild(P1, P2),
    Degree = 'grand child'.

relationship(Degree, P1, P2) :-
    grandFather(P1, P2),
    Degree = 'grand father'.

relationship(Degree, P1, P2) :-
    grandMother(P1, P2),
    Degree = 'grand mother'.

relationship(Degree, P1, P2) :-
    brother(P1, P2),
    Degree = 'brother'.

relationship(Degree, P1, P2) :-
    sister(P1, P2),
    Degree = 'sister'.

relationship(Degree, P1, P2) :-
    uncle(P1, P2),
    Degree = 'uncle'.

relationship(Degree, P1, P2) :-
    aunt(P1, P2),
    Degree = 'aunt'.

relationship(Degree, P1, P2) :-
    nephew(P1, P2),
    Degree = 'nephew'.

relationship(Degree, P1, P2) :-
    cousine(P1, P2),
    Degree = 'cousine'.

relationship(Degree, P1, P2) :-
    married(P2, P1),
    Degree = 'husband'.

relationship(Degree, P1, P2) :-
    married(P1, P2),
    Degree = 'wife'.

relationship(Degree, P1, P2) :-
    motherInLaw(P1, P2),
    Degree = 'mother-in-law'.

relationship(Degree, P1, P2) :-
    sonInLaw(P1, P2),
    Degree = 'son-in-law'.
```

Отдельно определим предикат, который будет использоваться для дальних родственников. В `Degree` будет записано то, насколько поколений один человек старше другого.

```prolog
relationshipBFS(Degree, P1, P2) :-
    bfsStart(P1, P2, Path, 7),
    length(Path, N),
    N > 0,
    N1 is N - 1,
    atomics_to_string([P1, ' is ', N1, ' generations older than ', P2], Degree).

relationshipBFS(Degree, P1, P2) :-
    bfsStart(P2, P1, Path, 7),
    length(Path, N),
    N > 0,
    N1 is N - 1,
    atomics_to_string([P2, ' is ', N1, ' generations older than ', P2], Degree).
```

Приведем примеры работы этих предикатов:
```shell
?- relationship(Degree, 'Ярополк I Святославич, вел.кн. Киевский', 'Олег Святославич, кн. Древлянский').
Degree = brother .

?- relationshipBFS(Degree, 'Рюрик I, кн. Новгородский', 'Владимир I Святославич Святой, вел.кн. Киевский').
Degree = "Рюрик I, кн. Новгородский is 3 generations older than Владимир I Святославич Святой, вел.кн. Киевский" .
```

## Естественно-языковый интерфейс

Перейдем к высокоуровневому предикату `request`. Именно с его помощью можно задавать вопросы на естественном языке.

Поддерживаются запросы вида:
- how many `Degree` did `Person` have
- who is `Person1` to `Person2`
- all `Degree` of `Person`
- did `Person` have `Degree`

Возможные вариации слов:
- did, does, do
- many, much
- have, has, had
- to, for

Примеры:
```shell
?- request([how, many, children, did, 'Владимир I Святославич Святой, вел.кн. Киевский', have]).
Владимир I Святославич Святой, вел.кн. Киевский had 16 children

?- request([who, is, 'Ярополк I Святославич, вел.кн. Киевский', to, 'Владимир I Святославич Святой, вел.кн. Киевский']).
Ярополк I Святославич, вел.кн. Киевский is brother to Владимир I Святославич Святой, вел.кн. Киевский

?- request([all, wives, of, 'Владимир I Святославич Святой, вел.кн. Киевский']).
All wives:
N Йонингенская
Аделья
Анна Византийская
Малфрида
Олова
Рогнеда Рогволодовна, кнж. Полоцкая
Юлия Греческая

?- request([all, parents, of, 'Владимир I Святославич Святой, вел.кн. Киевский']).
All parents:
Малуша
Святослав I, вел.кн. Киевский

?- request([did, 'Малуша', have, 'mother-in-law']).
Малуша had mother-in-law

?- request([did, 'Владимир I Святославич Святой, вел.кн. Киевский', have, sister]).
Владимир I Святославич Святой, вел.кн. Киевский didn't have sister
```

Пусть соответствующие предикаты принимают список слов и определяют, подходят ли они вопросу. Имея единый интерфейс обращения, мы можем реализовать универсальный предикат `request`, при помощи которого будет определяться нужный запрос.

```prolog
whoIsTo([Who, Is, P1, To, P2]) :-
    who(Who), is(Is), to(To),
    relationship(Res, P1, P2), !,
    write(P1), write(' is '), write(Res), write(' to '), write(P2), write('\n').

whoIsTo([Who, Is, P1, To, P2]) :-
    who(Who), is(Is), to(To),
    relationshipBFS(Res, P1, P2),
    write(Res).

whoIsTo([Who, Is, P1, To, P2]) :-
    who(Who), is(Is), to(To),
    atomics_to_string([P1, ' is not in relation with ', P2], R),
    write(R).

howMany([How, Many, Degrees, Did, P, Have]) :-
    how(How), many(Many), did(Did), have(Have),
    pluralToSingular(Degrees, Degree),
    countRelatives(Degree, P, Res),
    atomics_to_string([P, ' had ', Res, ' ', Degrees], R),
    write(R).

didHave([Did, P, Have, Degree]) :-
    did(Did), have(Have), 
    relationship(Degree, _, P),
    atomics_to_string([P, ' had ', Degree], R),
    write(R).

didHave([Did, P, Have, Degree]) :-
    did(Did), have(Have), 
    atomics_to_string([P, " didn't have ", Degree], R),
    write(R).

allOf([All, Degrees, Of, P]) :- 
    all(All), of(Of),
    pluralToSingular(Degrees, Degree),
    setof(P1, relationship(Degree, P1, P), R),
    atomics_to_string(['All ', Degrees, ':\n'], H),
    write(H),
    writeList(R).

request(Words) :-
    whoIsTo(Words).

request(Words) :-
    howMany(Words).

request(Words) :-
    didHave(Words).

request(Words) :-
    allOf(Words).
```

Для наибольшего удобства использования сделаем бесконечный цикл, в котором будем считывать список слов вопроса.

```prolog
loop :-
    write('Ask questions in format "[my, cool, question]."\n'),
    repeat,
    read(Words),
    request(Words),
    write('\n'),
    fail.
```

```shell
?- loop.
Ask questions in format "[my, cool, question]."
|: [did, 'Владимир I Святославич Святой, вел.кн. Киевский', have, sister].
Владимир I Святославич Святой, вел.кн. Киевский didn't have sister
|: [all, parents, of, 'Владимир I Святославич Святой, вел.кн. Киевский'].
All parents:
Малуша
Святослав I, вел.кн. Киевский
```

## Выводы

Вследствие выполения курсового проекта я реализовал предикаты для определения родства двух людей, подсчета и нахождения всех родственников одного типа.

В процессе работы я изучил принципы логического программирования. Мне показалось достаточно необычным написание программы на декларативном языке. Приятно осознавать, что не нужно в явном виде задавать инструкции компьютеру, достаточно описать основные верные утверждения.

Код программы получается достаточно компактным и читаемым, что очень удобно для работы в команде.

Однако стоит отметить, что производительность Prolog ниже, чем у императивных языков, так как нет полного контроля над выполнением программы.

Таким образом, исходя из всего вышесказанного, можно прийти к выводу, что Prolog удобно использовать в сферах, где необходимо описывать большое количество утверждений и следить за их исполнением (например, для проверки каких-либо стандартов), где производительность программы не стоит во главе угла. 