/*
  Schedule candidates with panels
  To get possible matchign slots 
 ?- matching(P, C, D, S).
  For specific candidate, call (for all possible options)
 ?- matching(P, cx, D, S).


*/
candidate(c1).
candidate(c2).

panel(p1).
panel(p2).
panel(p3).

knows(p1, java).
knows(p2, java).
knows(p1, data).
knows(p1, react).
knows(c1, java).
knows(c2, react).

/* p1 signed up for one slot */
pref_slot(p1, date(2021, 12, 1), 1).
/* p2 signed up for two slots */
pref_slot(p2, date(2021, 12, 1), 1).
pref_slot(p2, date(2021, 12, 1), 2).
/* p3 signed up for one */
pref_slot(p3, date(2021, 12, 1), 2).

/* candidate section */
pref_slot(c1, date(2021, 12, 1), 1).
/* c2 is ok with two slots */
pref_slot(c2, date(2021, 12, 1), 1).
pref_slot(c2, date(2021, 12, 1), 2).

/* Once we schedule an interview - we make it formal here */
scheduled(p1, c1, date(2021, 12, 1), 1).

mapped(P, C, D, S) :- panel(P), candidate(C), pref_slot(P, D, S), pref_slot(C, D, S), scheduled(P, C, D, S).

free_panel(P,D,S) :- panel(P), pref_slot(P, D, S), \+ mapped(P, _, D, S).

avail_candidate(C, D, S) :- candidate(C), pref_slot(C, D, S), \+ mapped(_, C, D, S).

matching(P, C, D, S) :- free_panel(P, D, S), avail_candidate(C, D, S).

notmapped(C) :- \+ avail_candidate(C, _, _).


