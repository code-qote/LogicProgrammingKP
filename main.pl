:- [tree].

male(Person) :- father(Person, _).
female(Person) :- mother(Person, _).

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

motherInLaw(MotherInLaw, SonInLaw) :- married(Wife, SonInLaw), mother(MotherInLaw, Wife).
sonInLaw(SonInLaw, MotherInLaw) :- motherInLaw(MotherInLaw, SonInLaw).

prolong([X|T], [Y,X|T]) :- child(Y, X).

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

countRelatives(Degree, P1, Res) :- 
    setof(P2, relationship(Degree, P2, P1), R),
    length(R, Res).

who('who').
is('is').
to('to').
to('for').

how('how').
many('many').
many('much').
did('did').
did('does').
did('do').
have('have').
have('had').
have('has').

all('all').
of('of').

pluralToSingular('children', 'child') :- !.
pluralToSingular('grand children', 'grand child') :- !.
pluralToSingular('wives', 'wife') :- !.
pluralToSingular(Words, Word) :-
    atom_length(Words, N),
    N1 is N - 1,
    sub_atom(Words, 0, N1, _, Word).

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

writeList([]).
writeList([H|T]) :-
    atomics_to_string([H, '\n'], R),
    write(R),
    writeList(T).

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

loop :-
    write('Ask questions in format "[my, cool, question]."\n'),
    repeat,
    read(Words),
    request(Words),
    write('\n'),
    fail.

