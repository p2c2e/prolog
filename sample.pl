
p(123,chennai, ast).
p(234, chennai, tsg).
p(345, pune, ast).
p(456, pune, ast).
p(567, chennai, tsg).
p(890, chennai, eagle).
p(678, pune, per).
p(789, pune, ct).



divide(_,N, []) :-
    N < 1.
divide(Items, 1, [Items]).
divide(Items, N, [Selected|Other]) :-
    N > 1,
    powset3(Items, Selected, Rest),
    N1 is N-1,
    divide(Rest, N1, Other).

ziptogether([], [], [] ).
ziptogether( [HA |TA] , [HB |TB] , [together(HA, HB)|TC] ) :- 
   ziptogether(TA,TB,TC).

getBagConfig(Proposal) :-
   findall(p(I,C,S),p(I,C,S),Is), 
   findall(team(A,B), team(A,B), Ts), 
   length(Ts,NB),
   divide(Is,NB,Ds),
   ziptogether(Ts,Ds,Proposal).

% findall(p(I,C,S),p(I,C,S),Is), findall(team(A,B), team(A,B), Ts), length(Ts,NB). 

% findall(I,item(I),Is),
% findall(B,bag(B),Bs),
% length(Bs,NB),
% % findall(Proposal,(divide(Is,NB,Ds),ziptogether(Bs,Ds,Proposal)} List).

%  use_rendering(table,        [header(team('Id1', 'Id2'))]).

comb2(_,[]).
comb2([X|T],[X|Comb]):-comb2(T,Comb).
comb2([_|T],[X|Comb]):-comb2(T,[X|Comb]).

tmp(Is,I1,I2) :-
   comb2(Is,[X,Y]), 
   p(I1,C1,S1) = X, 
   p(I2,C1,S2) = Y,
      I1 \= 12,
   S1 \= S2.
% findall(p(I,C,S),p(I,C,S),Is),
% findall([I1,I2], tmp(Is,I1,12), All).

filterit([],[]).
filterit([X],[X]).
filterit([X|Y],New) :-
    [I1, I2] = X,
   \+member([_,I1],New), 
   \+member([I2,_],New), 
   append([X],Other,New), 
   filterit(Y,Other).

% findall(p(I,C,S),p(I,C,S),IS),
%findall([11,I2], tmp(Is,I1,I2), All), 
%filterit(All,New).

% findall(p(I,C,S),p(I,C,S),Is), comb2(Is,[X,Y]), p(I1,C1,S1) = X, p(I2,C1,S2) = Y, S1 \= S2.

comball(Is, I1, I2) :- 
   comb2(Is,[X,Y]), 
   p(I1,C1,S1) = X, 
   p(I2,C1,S2) = Y,
      I1 \= 12,
   S1 \= S2.

team(A,B) :-
   p(A, Cl, S1),
   p(B, Cl, S2), % same city
   A \= B, % different people
   S2 \= S1. % from different sectors

teams(Ts) :-
   length(Ts, 4),
   member(team(_,_) , Ts).

xteams(Ts) :-
   length(Ts, 4),
   member([p(A, C1, Sl),p(B, C1, S2)], Ts), 
   A \= B, % different people
   S2 \= Sl. %- from different sectors


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pairs( [X | T] , PS) :- T= [_|_] , pairs(X, T, T, PS, [] ); T= [] , PS= [].
pairs( [], [] ).
pairs(_, [] , [] ,Z, Z).
pairs(_, [], [X|T], PS, Z) :- pairs(X, T, T, PS, Z).
pairs(X, [Y|T] , R, [X-Y | PS] , Z) :- pairs(X, T ,R, PS, Z).

% pairs( [1,2, 3, 4] , I)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
