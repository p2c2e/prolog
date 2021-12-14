/*
 Run this as :
 ?- [csched_code, csched_data].
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
:- discontiguous assert_prefs/4.


/* Start */
:- assert_candidate(lav2, cs_lvl(sm, data)).
:- assert_prefs(lav2, date(2021, 12, 14), 1, 10).

:- assert_candidate(ashish, cs_lvl(al2, data)).
:- assert_prefs(ashish, date(2021, 12, 14), 3, 5).

:- assert_candidate(ankit, cs_lvl(al2, data)).
:- assert_prefs(ankit, date(2021, 12, 15), 2, 5).

/*
candidate(ankit).
knows(ankit, cs_lvl(al2, data)).
pref_slot(ankit, date(2021, 12, 15), 2).
pref_slot(ankit, date(2021, 12, 15), 3).
pref_slot(ankit, date(2021, 12, 15), 4).
pref_slot(ankit, date(2021, 12, 15), 5).
*/

:- assert_candidate(trishna, cs_lvl(al2, data)).
:- assert_prefs(trishna, date(2021, 12, 17), 3, 4).

:- assert_candidate(dinesh, cs_lvl(al2, data)).
:- assert_prefs(dinesh, date(2021, 12, 14), 4, 6).

:- assert_candidate(ankur, cs_lvl(mgr, data)).
:- assert_prefs(ankur, date(2021, 12, 13), 4, 5).
:- assert_prefs(ankur, date(2021, 12, 14), 4, 5).


:- assert_candidate(roshan, cs_lvl(al2, data)).
:- assert_prefs(roshan, date(2021, 12, 15), 3, 6).

:- assert_candidate(lavanya, cs_lvl(al2, data)).
:- assert_prefs(lavanya, date(2021, 12, 14), 1, 6).

:- assert_candidate(utkarsha, cs_lvl(al2, data)).
:- assert_prefs(utkarsha, date(2021, 12, 14), 11, 12).
:- assert_prefs(utkarsha, date(2021, 12, 14), 1, 1).
:- assert_prefs(utkarsha, date(2021, 12, 14), 5, 6).


:- assert_candidate(dharana, cs_lvl(sal2, data)).
:- assert_prefs(dharana, date(2021, 12, 14), 11, 12).
:- assert_prefs(dharana, date(2021, 12, 14), 1, 3).

:- assert_candidate(neetu, cs_lvl(sal2, data)).
:- assert_prefs(neetu, date(2021, 12, 15), 2, 4).
:- assert_prefs(neetu, date(2021, 12, 16), 2, 4).

:- assert_candidate(nishant, cs_lvl(sal1, data)).
:- assert_prefs(nishant, date(2021, 12, 15), 3, 3).
:- assert_prefs(nishant, date(2021, 12, 17), 3, 3).
:- assert_prefs(nishant, date(2021, 12, 18), 1, 3).


/* There is Sanjay's AMA going on in this slot */
/*
blacklist(date(2021, 12, 10), 3). 
*/


/* Once we schedule an interview - we make it formal here 
   Once mapped, they will not appear in potential matches 
*/
/*
scheduled(p1, c1, date(2021, 12, 1), 1).
*/
scheduled(p3, zoheb, date(2021, 12, 1), 5).

/* End */
:- assert_panel(p_akarsh, cs_lvl(al2, data)).
:- assert_prefs(p_akarsh, date(2021, 12, 14), 4, 4).
:- assert_prefs(p_akarsh, date(2021, 12, 15), 4, 4).
:- assert_prefs(p_akarsh, date(2021, 12, 17), 4, 4).

:- assert_panel(p_rahulmittal, cs_lvl(al2, data)).
:- assert_prefs(p_rahulmittal, date(2021, 12, 18), 11, 12).
:- assert_prefs(p_rahulmittal, date(2021, 12, 18), 2, 2).
:- assert_prefs(p_rahulmittal, date(2021, 12, 18), 4, 4).


:- assert_panel(p_reddivadla, cs_lvl(al2, data)).
:- assert_prefs(p_reddivadla, date(2021, 12, 18), 10, 12).
:- assert_prefs(p_reddivadla, date(2021, 12, 18), 2, 2).
:- assert_prefs(p_reddivadla, date(2021, 12, 18), 4, 4).

:- assert_panel(p_narendarsharma, cs_lvl(sal1, data)).
:- assert_prefs(p_narendarsharma, date(2021, 12, 14), 4, 5).
:- assert_prefs(p_narendarsharma, date(2021, 12, 15), 4, 5).

:- assert_panel(p_puneetkukreja, cs_lvl(sal1, data)).
:- assert_prefs(p_puneetkukreja, date(2021, 12, 15), 3, 3).
:- assert_prefs(p_puneetkukreja, date(2021, 12, 16), 3, 3).

:- assert_panel(p_rajasree, cs_lvl(sal1, data)).
:- assert_prefs(p_rajasree, date(2021, 12, 14), 2, 2).
:- assert_prefs(p_rajasree, date(2021, 12, 14), 11, 11).
:- assert_prefs(p_rajasree, date(2021, 12, 15), 3, 3).
:- assert_prefs(p_rajasree, date(2021, 12, 16), 3, 3).

:- assert_panel(p_abhinavdubey, cs_lvl(sal1, data)).
:- assert_prefs(p_abhinavdubey, date(2021, 12, 15), 11, 11).
:- assert_prefs(p_abhinavdubey, date(2021, 12, 16), 11, 11).

:- assert_panel(p_ankush, cs_lvl(sal1, data)).
:- assert_prefs(p_ankush, date(2021, 12, 16), 11, 11).

:- assert_panel(p_anubeig, cs_lvl(sal1, data)).
:- assert_prefs(p_anubeig, date(2021, 12, 18), 9, 12).
:- assert_prefs(p_anubeig, date(2021, 12, 18), 2, 4).

:- assert_panel(p_bikesh, cs_lvl(sal1, data)).
:- assert_prefs(p_bikesh, date(2021, 12, 18), 11, 12).
:- assert_prefs(p_bikesh, date(2021, 12, 18), 1, 1).
:- assert_prefs(p_bikesh, date(2021, 12, 18), 3, 8).


:- assert_panel(p_chandra, cs_lvl(sal1, data)).
:- assert_prefs(p_chandra, date(2021, 12, 15), 10, 10).
:- assert_prefs(p_chandra, date(2021, 12, 16), 10, 10).
:- assert_prefs(p_chandra, date(2021, 12, 17), 10, 10).

:- assert_panel(p_monika, cs_lvl(sal1, data)).
:- assert_prefs(p_monika, date(2021, 12, 18), 12, 12).

:- assert_panel(p_saurabhsingh, cs_lvl(sal1, data)).
:- assert_prefs(p_saurabhsingh, date(2021, 12, 18), 9, 12).
:- assert_prefs(p_saurabhsingh, date(2021, 12, 18), 1, 5).


:- assert_panel(p_vineetjha, cs_lvl(sal2, data)).
:- assert_prefs(p_vineetjha, date(2021, 12, 14), 4, 4).
:- assert_prefs(p_vineetjha, date(2021, 12, 15), 4, 4).
:- assert_prefs(p_vineetjha, date(2021, 12, 16), 4, 4).
:- assert_prefs(p_vineetjha, date(2021, 12, 17), 4, 4).

:- assert_panel(p_amitjha, cs_lvl(sal2, data)).
:- assert_prefs(p_amitjha, date(2021, 12, 18), 10, 11).
:- assert_prefs(p_amitjha, date(2021, 12, 18), 1, 1).
:- assert_prefs(p_amitjha, date(2021, 12, 15), 11, 12).
:- assert_prefs(p_amitjha, date(2021, 12, 16), 11, 12).
:- assert_prefs(p_amitjha, date(2021, 12, 17), 11, 12).

:- assert_panel(p_anilsingh, cs_lvl(sal2, data)).
:- assert_prefs(p_anilsingh, date(2021, 12, 16), 3, 3).
:- assert_prefs(p_anilsingh, date(2021, 12, 17), 3, 3).

:- assert_panel(p_shasankar, cs_lvl(sal2, data)).
:- assert_prefs(p_shasankar, date(2021, 12, 15), 4, 4).
:- assert_prefs(p_shasankar, date(2021, 12, 16), 4, 4).
:- assert_prefs(p_shasankar, date(2021, 12, 17), 4, 4).

:- assert_panel(p_sheel, cs_lvl(sal2, data)).
:- assert_prefs(p_sheel, date(2021, 12, 18), 11, 11).
:- assert_prefs(p_sheel, date(2021, 12, 18), 4, 4).

:- assert_panel(p_sridharan, cs_lvl(sal2, data)).
:- assert_prefs(p_sridharan, date(2021, 12, 15), 3, 3).
:- assert_prefs(p_sridharan, date(2021, 12, 17), 3, 3).

:- assert_panel(p_sunilnair, cs_lvl(sal2, data)).
:- assert_prefs(p_sunilnair, date(2021, 12, 15), 10, 10).


:- assert_panel(p_vinu, cs_lvl(sal2, data)).
:- assert_prefs(p_vinu, date(2021, 12, 18), 1, 1).


