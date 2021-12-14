/*

Load file with 
?- [schedule4]. 
Recompile with
?- make.
And halt with
?- halt.

  Schedule candidates with panels
  To get possible matchign slots 
 ?- matching(P, C, D, S).
  For specific candidate, call (for all possible options)
 ?- matching(P, cx, D, S).


*/
:- discontiguous(candidate/1).
:- dynamic candidate/1.
:- discontiguous(panel/1).
:- dynamic panel/1.
:- discontiguous(knows/2).
:- dynamic knows/2.
:- discontiguous(pref_slot/3).
:- dynamic pref_slot/3.
:- dynamic blacklist/2.


mapped(P, C, D, S) :- panel(P), candidate(C), pref_slot(P, D, S), pref_slot(C, D, S), scheduled(P, C, D, S).

free_panel(P,D,S) :- panel(P), pref_slot(P, D, S), \+ mapped(P, _, D, S), \+ blacklist(D, S).

avail_candidate(C, D, S) :- candidate(C), pref_slot(C, D, S), \+ mapped(_, C, D, S), \+ blacklist(D, S).

matching(P, C, D, S) :- free_panel(P, D, S), avail_candidate(C, D, S), knows(P, Skill), knows(C, Skill).

notmapped(C) :- \+ avail_candidate(C, _, _).


/* Start */

candidate(ashish).
knows(ashish, cs_lvl(al2, data)).
pref_slot(ashish, date(2021, 12, 14), 3).
pref_slot(ashish, date(2021, 12, 14), 4).
pref_slot(ashish, date(2021, 12, 14), 5).

candidate(ankit).
knows(ankit, cs_lvl(al2, data)).
pref_slot(ankit, date(2021, 12, 15), 2).
pref_slot(ankit, date(2021, 12, 15), 3).
pref_slot(ankit, date(2021, 12, 15), 4).
pref_slot(ankit, date(2021, 12, 15), 5).

candidate(trishna).
knows(trishna, cs_lvl(al2, data)).
pref_slot(ankit, date(2021, 12, 17), 3).
pref_slot(ankit, date(2021, 12, 17), 4).

candidate(dinesh).
knows(dinesh, cs_lvl(al2, data)).
pref_slot(dinesh, date(2021, 12, 14), 4).
pref_slot(dinesh, date(2021, 12, 14), 5).
pref_slot(dinesh, date(2021, 12, 14), 6).

candidate(ankur).
knows(ankur, cs_lvl(mgr, data)).
pref_slot(ankur, date(2021, 12, 13), 4).
pref_slot(ankur, date(2021, 12, 13), 5).
pref_slot(ankur, date(2021, 12, 14), 4).
pref_slot(ankur, date(2021, 12, 14), 5).

candidate(roshan).
knows(roshan, cs_lvl(al2, data)).
pref_slot(roshan, date(2021, 12, 15), 3).
pref_slot(roshan, date(2021, 12, 15), 4).
pref_slot(roshan, date(2021, 12, 15), 5).
pref_slot(roshan, date(2021, 12, 15), 6).

candidate(lavanya).
knows(lavanya, cs_lvl(al2, data)).
pref_slot(roshan, date(2021, 12, 14), 1).
pref_slot(roshan, date(2021, 12, 14), 2).
pref_slot(roshan, date(2021, 12, 14), 3).
pref_slot(roshan, date(2021, 12, 14), 4).
pref_slot(roshan, date(2021, 12, 14), 5).
pref_slot(roshan, date(2021, 12, 14), 6).

:- discontiguous assert_prefs/4.

assert_prefs(Name, Date, End, End) :-
  \+ pref_slot(Name, Date, End), 
  assertz(pref_slot(Name, Date, End)).

assert_prefs(Name, Date, Start, End) :-
  Start =\= End,
  \+ pref_slot(Name, Date, Start), 
  assertz(pref_slot(Name, Date, Start)),
  N is Start+1,
  assert_prefs(Name, Date, N, End).
    
assert_candidate(Name, Lvl) :-
  \+ candidate(Name),
  assertz(candidate(Name)),
  \+ knows(Name, Lvl),
  assertz(knows(Name, Lvl)).
/*
  assert_prefs(Name, Date, Start, End).
*/

assert_candidate(lav2, cs_lvl(sm, data)).
assert_prefs(lav2, date(2021, 12, 14), 1, 10).

/* Loop logic ... 
assert_candidate(Name, Lvl) :-
  assertz(candidate(Name)),
  assertz(knows(Name, Lvl)).
assert_candidate(lav2, cs_lvl(sm, data)).

outn(L, L) :- atom(L).
outn(F, L) :- F =\= L, atom(F), N is F+1, outn(N, L).
*/

candidate(utkasha).
knows(utkarsha, cs_lvl(al2, data)).
pref_slot(utkarsha, date(2021, 12, 14), 11).
pref_slot(utkarsha, date(2021, 12, 14), 12).
pref_slot(utkarsha, date(2021, 12, 14), 1).
pref_slot(utkarsha, date(2021, 12, 14), 5).
pref_slot(utkarsha, date(2021, 12, 14), 6).

candidate(dharana).
knows(dharana, cs_lvl(sal2, data)).
pref_slot(dharana, date(2021, 12, 14), 11).
pref_slot(dharana, date(2021, 12, 14), 12).
pref_slot(dharana, date(2021, 12, 14), 1).
pref_slot(dharana, date(2021, 12, 14), 2).
pref_slot(dharana, date(2021, 12, 14), 3).

candidate(neetu).
knows(neetu, cs_lvl(sal2, data)).
pref_slot(neetu, date(2021, 12, 15), 2).
pref_slot(neetu, date(2021, 12, 15), 3).
pref_slot(neetu, date(2021, 12, 15), 4).
pref_slot(neetu, date(2021, 12, 16), 2).
pref_slot(neetu, date(2021, 12, 16), 3).
pref_slot(neetu, date(2021, 12, 16), 4).

candidate(nishant).
knows(nishant, cs_lvl(sal1, data)).
pref_slot(nishant, date(2021, 12, 15), 3).
pref_slot(nishant, date(2021, 12, 17), 3).
pref_slot(nishant, date(2021, 12, 18), 1).
pref_slot(nishant, date(2021, 12, 18), 2).
pref_slot(nishant, date(2021, 12, 18), 3).

panel(p_akarsh).
knows(p_akarsh, cs_lvl(al2, data)).
pref_slot(p_akarsh, date(2021, 12, 14), 4).
pref_slot(p_akarsh, date(2021, 12, 15), 4).
pref_slot(p_akarsh, date(2021, 12, 17), 4).

/* There is Sanjay's AMA going on in this slot */
/*
blacklist(date(2021, 12, 10), 3). 
*/

/* p1 signed up for one slot */
pref_slot(p1, date(2021, 12, 1), 1).
/* p2 signed up for two slots */
pref_slot(p2, date(2021, 12, 1), 1).
pref_slot(p2, date(2021, 12, 1), 2).
/* p3 */
pref_slot(p3, date(2021, 12, 1), 2).
pref_slot(p3, date(2021, 12, 1), 3).
pref_slot(p3, date(2021, 12, 1), 4). /* Useless - since it is blacklisted */
pref_slot(p3, date(2021, 12, 1), 5).
pref_slot(p3, date(2021, 12, 1), 6).
pref_slot(p3, date(2021, 12, 1), 7).
pref_slot(sudhan, date(2021, 12, 10), 1).
pref_slot(sudhan, date(2021, 12, 10), 2).
pref_slot(sudhan, date(2021, 12, 10), 3).

pref_slot(ariti, date(2021, 12, 10), 3).
pref_slot(ariti, date(2021, 12, 10), 5).

pref_slot(zoheb, date(2021, 12, 1), 5).


/* candidate section */
pref_slot(c1, date(2021, 12, 1), 1).
/* c2 is ok with two slots */
pref_slot(c2, date(2021, 12, 1), 1).
pref_slot(c2, date(2021, 12, 1), 2).

pref_slot(c3, date(2021, 12, 1), 5).

/* Once we schedule an interview - we make it formal here 
   Once mapped, they will not appear in potential matches 
*/
/*
scheduled(p1, c1, date(2021, 12, 1), 1).
*/
scheduled(p3, zoheb, date(2021, 12, 1), 5).

/* End */
