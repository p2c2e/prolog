/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   An improved SAT formulation for the social golfer problem.
   Written by Markus Triska (triska@metalevel.at), 2008-2020
   Public domain code.

   G*P golfers play in G groups of P players for W weeks such that no
   two golfers play in the same group more than once.

   This Prolog program generates a SAT model in DIMACS format for the
   given instance specified by the parameters G, P, and W.

   Example:

       ?- G-P-W = 2-2-2, golf(G,P,W).
       %@ p cnf 48 254
       %@ 17 19 21 23 0
       %@ 18 20 22 24 0
       %@ 25 27 29 31 0
       %@ 26 28 30 32 0
       %@ etc.


   You can invoke Scryer Prolog as follows to put the (CNF-)formula
   into a file, say g532.txt:

       $ scryer-prolog -g "golf(5,3,2),halt" satgolf.pl > g532.txt

   You then feed this to a SAT solver to solve the instance (if possible).

   For example, using Walksat:

       $ walksat -out g532.sol < g532.txt

   In g532.sol, you then find a *solution*, i.e., an assignment that
   makes the SAT formula true.

   You can turn this into a Prolog *list* for example with Emacs. Evaluate
   the following definition, then select the content of g532.sol and do:

       M-x golfer-format-solution RET

   on the content. Its definition is:

       (defun golfer-format-solution (start end)
          (interactive "r")
          (save-excursion
            (save-restriction
              (narrow-to-region start end)
              (goto-char (point-min))
              (replace-string "\n" " ")
              (goto-char (point-min))
              (replace-regexp "\-[0-9]+" "")
              (goto-char (point-min))
              (replace-regexp "\\s +" " ")
              (goto-char (point-min))
              (delete-horizontal-space)
              (insert "[")
              (goto-char (point-max))
              (delete-horizontal-space)
              (insert "]")
              (goto-char (point-min))
              (replace-string " " ","))))

   For example, g532.sol may start like this:

         1 2 -3 -4 -5 -6 -7 -8 -9 -10
         11 -12 -13 14 -15 -16 -17 -18 -19 -20
         -21 -22 23 -24 -25 26 -27 -28 -29 -30
         -31 -32 33 34 -35 -36 -37 -38 -39 -40
         -41 -42 -43 44 45 -46 -47 -48 -49 -50
         etc.

   And golfer-format-solution will turn this into a list like:

        [1,2,11,14,23,26,33,34,44, ...]

   You can provide this list of *positive* literals to schedule/4 to print
   the solution in readable form:

       ?- schedule(5, 3, 2, [1,2,11,14,23,26,33,34,44, ...]).
       %@ schedule([
       %@    % week 1:
       %@    [[1, 2, 14], [3, 4, 10], [5, 8, 12], [6, 9, 13], [7, 11, 15]],
       %@    % week 2:
       %@    [[1, 7, 12], [2, 4, 5], [3, 6, 8], [9, 10, 15], [11, 13, 14]]
       %@ ]).
       %@ true.


   For more information about the social golfer problem, visit:

                    https://www.metalevel.at/sgp/
                    =============================

   Please write to triska@metalevel.at if you have any questions.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpfd)). % was clpz
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(between)).

:- op(300, xfy, v).
:- op(300, xfy, =>).
:- op(550, xfx, to).

make_lookup(Vs, Ins, Lookup) :-
        findall(Vs,
                ( maplist(call, Ins), term_variables(Ins,Vars),label(Vars)),
                Vss),
        append(Vss, Vss1),
        sort(Vss1, Vss2),
        empty_assoc(Lookup0),
        make_lookup_(Vss2, 1, Lookup0, Lookup).

make_lookup_([], _, A, A).
make_lookup_([V|Vs], N0, A0, A) :-
        put_assoc(V, A0, N0, A1),
        N1 #= N0 + 1,
        make_lookup_(Vs, N1, A1, A).

golf(G, P, W) :-
        X #= P * G,
        make_lookup([g(I,J,K,L),g(I,K,L)], [I in 1..X, L in 1..W,
                                            K in 1..G, J in 1..P], L),
        assoc_to_list(L, Ls),
        length(Ls, Order),
        bb_b_put(counting, true),
        bb_b_put(num_clauses, 0),
        model(G, P, W, X, L),
        bb_get(num_clauses, NumClauses),
        format("p cnf ~w ~w\n", [Order,NumClauses]),
        bb_b_put(counting, false),
        model(G, P, W, X, L).

model(G, P, W, X, L) :-
        bb_put(at_newline, true),
        % each player plays at least once each week
        emit(^(i = 1 to X,
               ^(l = 1 to W,
                 v(j = 1 to P,
                   v(k = 1 to G,
                     g(i,j,k,l))))), L),
        % each player plays at most once in the same group
        emit(^(i = 1 to X,
               ^(l = 1 to W,
                 ^(j = 1 to P,
                   ^(k = 1 to G,
                     ^(m = j+1 to P,
                       g(i,j,k,l) => \g(i,m,k,l)))))), L),
        % each player plays at most once per week
        emit(^(i = 1 to X,
               ^(l = 1 to W,
                 ^(j = 1 to P,
                   ^(k = 1 to G,
                     ^(m = k+1 to G,
                       ^(n = 1 to P,
                         g(i,j,k,l) => \g(i,n,m,l))))))), L),
        % in each group, at least one player is the j-th golfer
        emit(^(l = 1 to W,
               ^(k = 1 to G,
                 ^(j = 1 to P,
                   v(i = 1 to X,
                     g(i,j,k,l))))), L),
        % in each group, at most one player is the j-th golfer
        emit(^(l = 1 to W,
               ^(k = 1 to G,
                 ^(j = 1 to P,
                   ^(i = 1 to X,
                     ^(m = i+1 to X,
                       g(i,j,k,l) => \g(m,j,k,l)))))), L),
        % g(i,k,l): player i plays in group k of week l
        % g(i,j,k,l) => g(i,k,l) for all j
        emit(^(l = 1 to W,
               ^(k = 1 to G,
                 ^(j = 1 to P,
                   ^(i = 1 to X,
                     g(i,j,k,l) => g(i,k,l))))), L),
        % g(i,k,l) => g(i,j,k,l) for some j
        emit(^(l = 1 to W,
               ^(k = 1 to G,
                 ^(i = 1 to X,
                   g(i,k,l) => v(j = 1 to P,
                                 g(i,j,k,l))))), L),
        % g(x,k,l) ^ g(y,k,l) => -(g(x,k',l') ^ g(y,k',l'))
        emit(^(l = 1 to W,
               ^(k = 1 to G,
                 ^(m = 1 to X,
                   ^(n = m+1 to X,
                     ^(kp = 1 to G,
                       ^(lp = l+1 to W,
                         g(m,k,l) ^ g(n,k,l) =>
                           \ (g(m,kp,lp) ^ g(n,kp,lp)))))))), L),
        % symmetry breaking constraints
        emit(^(i = 1 to X,
               ^(j = 1 to P-1,
                 ^(k = 1 to G,
                   ^(l = 1 to W,
                     ^(m = 1 to i,
                       g(i,j,k,l) => \g(m,j+1,k,l)))))), L),
        emit(^(i = 1 to X,
               ^(k = 1 to G-1,
                 ^(l = 1 to W,
                   ^(m = 1 to i,
                     g(i,1,k,l) => \g(m,1,k+1,l))))), L),
        emit(^(i = 1 to X,
               ^(l = 1 to W-1,
                 ^(m = 1 to i,
                   g(i,2,1,l) => \g(m,2,1,l+1)))), L),
        nl_if_necessary.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Compile expression to DIMACS format emitted on stdout
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

emit(Expr, L) :-
        empty_assoc(A0),
        eval_expr(Expr, L, A0, _).

eval_expr(N, _, _, N)             :- integer(N), !.
eval_expr(Var, _, A, Formula)     :-
        atom(Var), !,
        get_assoc(Var, A, Formula).
eval_expr(\ (A ^ B), L, A0, Result) :- !, % TODO: this is ad hoc
        eval_expr(\A v \B, L, A0, Result).
eval_expr(\F0, L, A0, \F) :- !,
        unless_counting(format("-")),
        eval_expr(F0, L, A0, F).
eval_expr(A+B, L, A0, N)          :- !,
        eval_expr(A, L, A0, A1),
        eval_expr(B, L, A0, B1),
        N #= A1 + B1.
eval_expr(A-B, L, A0, N)          :- !,
        eval_expr(A, L, A0, A1),
        eval_expr(B, L, A0, B1),
        N #= A1 - B1.
eval_expr(A*B, L, A0, N)          :- !,
        eval_expr(A, L, A0, A1),
        eval_expr(B, L, A0, B1),
        N #= A1 * B1.
eval_expr(^(Var = From to To, Expr), L, A, Formula) :- !,
        and(Var, From, To, Expr, L, A, Formula).
eval_expr(v(Var = From to To, Expr), L, A, Formula) :- !,
        or(Var, From, To, Expr, L, A, Formula).
eval_expr(v(X,Y), L, A, v(X1,Y1))                :- !,
        eval_expr(X, L, A, X1),
        eval_expr(Y, L, A, Y1).
eval_expr(X => Y, L, A, Result)                :- !,
        eval_expr(\X v Y, L, A, Result).
eval_expr(Term, L, A, var(Num))      :- !,
        Term =.. [Functor|Args],
        maplist(eval_expr_(L,A), Args, Args1),
        Variable =.. [Functor|Args1],
        (   get_assoc(Variable, L, Num) ->
            unless_counting(format("~w ", [Num])),
            bb_put(at_newline, false)
        ;   format("could not locate: ~w\n", [Variable]),
            halt
        ).


eval_expr_(L, A, E, F) :- eval_expr(E, L, A, F).

and(Var, From0, To0, Expr, L, A0, dummy) :-
        eval_expr(From0, L, A0, From),
        eval_expr(To0, L, A0, To),
        (   From > To -> true
        ;   To1 #= To + 1,
            put_assoc(Var, A0, From, A1),
            nl_if_necessary,
            eval_expr(Expr, L, A1, Expr1),
            From1 #= From + 1,
            and_from_to(From1, To1, Var, Expr, L, A0, Expr1, _)
        ).

and_from_to(N, N, _, _, _, _, F, F)         :- !.
and_from_to(N0, N, Var, Expr, L, A0, F0, F) :-
        put_assoc(Var, A0, N0, A1),
        nl_if_necessary,
        eval_expr(Expr, L, A1, Expr1),
        F1 = (F0^Expr1),
        N1 #= N0 + 1,
        and_from_to(N1, N, Var, Expr, L, A0, F1, F).

or(Var, From0, To0, Expr, L, A0, dummy) :-
        eval_expr(From0, L, A0, From),
        eval_expr(To0, L, A0, To),
        (   From > To -> true
        ;   To1 #= To + 1,
            put_assoc(Var, A0, From, A1),
            eval_expr(Expr, L, A1, Expr1),
            From1 #= From + 1,
            or_from_to(From1, To1, Var, Expr, L, A0, Expr1, _)
        ).

or_from_to(N, N, _, _, _, _, F, F)         :- !.
or_from_to(N0, N, Var, Expr, L, A0, F0, F) :-
        put_assoc(Var, A0, N0, A1),
        eval_expr(Expr, L, A1, Expr1),
        F1 = F0 v Expr1,
        N1 #= N0 + 1,
        or_from_to(N1, N, Var, Expr, L, A0, F1, F).

counting :- bb_get(counting, true).

nl_if_necessary :-
        (   bb_get(at_newline, true) -> true
        ;   (   counting ->
                bb_get(num_clauses, N0),
                N #= N0 + 1,
                bb_b_put(num_clauses, N)
            ;   format("0\n")
            ),
            bb_put(at_newline, true)
        ).

unless_counting(Goal) :-
        (   counting -> true
        ;   call(Goal)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Resulting schedule from a list of true variables; example:

   ?- schedule(2,2,2, [1,2,5,8,11,12,14,15,17,18,28,29,35,40,46,47]).
   %@ schedule([
   %@    % week 1:
   %@    [[1,2],[3,4]],
   %@    % week 2:
   %@    [[1,4],[2,3]]
   %@ ]).

   The list of true variables can be obtained for example from
   an assignment found with walksat: cat instance | walksat -sol
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


ignore(G) :-
        (   G -> true
        ;   true
        ).

schedule(G, P, W, Trues) :-
        X #= G*P,
        make_lookup([g(I,J,K,L),g(I,K,L)], [I in 1..X, L in 1..W,
                                            K in 1..G, J in 1..P], Lookup),
        assoc_to_list(Lookup, Ls1),
        maplist(flip, Ls1, Ls2),
        list_to_assoc(Ls2, Lookup1),
        collect_trues(Trues, Lookup1, Vars),
        format("schedule([\n"),
        ignore((between(1, W, L),
                format("   % week ~w:\n", [L]),
                format("   ["),
                ignore((between(1, G, K),
                        format("["),
                        ignore((between(1, P, J),
                                (   memberchk(g(I,J,K,L), Vars) -> true
                                ;   I = ' '
                                ),
                                format("~w", [I]),
                                comma_if(J < P),
                                false)),
                        format("]"),
                        comma_if(K < G),
                       false)),
                format("]"),
                comma_if(L < W),
                nl,
                false)),
        format("]).\n").

comma_if(Goal) :-
        (   Goal ->
            format(",")
        ;   true
        ).

collect_trues([], _, []).
collect_trues([T|Ts], A, [V|Vs]) :-
        (   get_assoc(T, A, V) -> true
        ;   format("could not fetch: ~w\n", [T]),
            V = any
        ),
        collect_trues(Ts, A, Vs).

flip(A-B, B-A).

format(Str) :- format(Str, []).