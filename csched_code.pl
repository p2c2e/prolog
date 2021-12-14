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

mapped(P, C, D, S) :- panel(P), candidate(C), pref_slot(P, D, S), pref_slot(C, D, S), scheduled(P, C, D, S).

free_panel(P,D,S) :- panel(P), pref_slot(P, D, S), \+ mapped(P, _, D, S), \+ blacklist(D, S).

avail_candidate(C, D, S) :- candidate(C), pref_slot(C, D, S), \+ mapped(_, C, D, S), \+ blacklist(D, S).

matching(P, C, D, S) :- free_panel(P, D, S), avail_candidate(C, D, S), knows(P, Skill), knows(C, Skill).

notmapped(C) :- \+ avail_candidate(C, _, _).

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

assert_panel(Name, Lvl) :-
  \+ panel(Name),
  assertz(panel(Name)),
  \+ knows(Name, Lvl),
  assertz(knows(Name, Lvl)).

create_panel(Name) :-
  ( \+ panel(Name) ->
    assertz(panel(Name)) ;
    true
  ).


create_knows(Name, Lvl) :-
  ( \+ knows(Name, Lvl) ->
    assertz(knows(Name, Lvl)) ;
    true
  ).

assert_panel2(Name, [CSH | []] ) :-
  write("HEY"),
  create_panel(Name),
  \+ knows(Name, cs_lvl(CSH, data)),
  create_knows(Name, cs_lvl(CSH, data)).

assert_panel2(Name, [CSH| CST]) :-
  write(CSH),
  write(CST),
  create_panel(Name),
  create_knows(Name, cs_lvl(CSH, data)),
  assert_panel2(Name, CST).

/* Loop logic ... 
assert_candidate(Name, Lvl) :-
  assertz(candidate(Name)),
  assertz(knows(Name, Lvl)).
assert_candidate(lav2, cs_lvl(sm, data)).

outn(L, L) :- atom(L).
outn(F, L) :- F =\= L, atom(F), N is F+1, outn(N, L).
*/


