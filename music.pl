:- use_module(library(random)).

%note(0).
%note(1).
%note(2).
%note(3).
%note(4).
%note(5).
%note(6).
%note(7).
%note(8).
%note(9).
%note(10).
%note(11).

name(0,c).
name(1,c_sharp).
name(2,d).
name(3,d_sharp).
name(4,e).
name(5,f).
name(6,f_sharp).
name(7,g).
name(8,a_flat).
name(9,a).
name(10,b_flat).
name(11,b).

note_number(Name,Num) :-
  name(Num,Name).

note(Note) :-
  Note = 1;
  Note = 2;
  Note = 4;
  Note = 5;
  Note = 7;
  Note = 9;
  Note = 10.

cantus_firmus(L) :-
  L = [2,5,4,2,7,5,9,7,5,4,2].

fifth_above(X,Y) :-
  D is X+7,
  Y is D mod 12.

fourth_above(X,Y) :-
  D is X+5,
  Y is D mod 12.

unison(X,Y) :-
  X = Y.

third_above(X,Y) :-
  D is X+4,
  Y is D mod 12.

sixth_above(X,Y) :-
  D is X+9,
  Y is D mod 12.

tritone(X,Y) :-
  D is X+6,
  Y is D mod 12.

jump(X,Y) :-
  X-Y > 5.

consonant(X,Y) :-
  third_above(X,Y);
  fifth_above(X,Y);
  sixth_above(X,Y);
  unison(X,Y).

perfect(X,Y) :-
  fifth_above(X,Y);
  unison(X,Y);
  fourth_above(X,Y).

terminal(X,Y) :-
  fifth_above(X,Y);
  unison(X,Y).

up_motion(C1,H1,C2,H2) :-
  C2 - C1 > 0,
  H2 - H1 > 0.

down_motion(C1,H1,C2,H2) :-
  C2 - C1 < 0,
  H2 - H1 < 0.
  
direct_motion(C1,H1,C2,H2) :-
  up_motion(C1,H1,C2,H2);
  down_motion(C1,H1,C2,H2).

invalid_motion(C1,H1,C2,H2) :-
  (direct_motion(C1,H1,C2,H2),
   perfect(C2,H2));
  jump(C1,C2);
  jump(H1,H2).
  

build_random_line([X1,X2|Xs], [Y1,Y2|Ys]) :-
  random(0,10,R),
  R<8,
  once((
  repeat,
  random(0,12,Y2),
  note(Y2), 
  not(invalid_motion(X1,Y1,X2,Y2)),
  consonant(X1,Y1),
  consonant(X2,Y2))),
  build_random_line([X2|Xs],[Y2|Ys]).

build_random_line([X1,X2|Xs], [Y1,Y2|Ys]) :-
  note(Y2), 
  not(invalid_motion(X1,Y1,X2,Y2)),
  consonant(X1,Y1),
  consonant(X2,Y2),
  build_random_line([X2|Xs],[Y2|Ys]).

build_random_line([X1,X2], [Y1,Y2]):-
  note(Y1), note(Y2),
  not(invalid_motion(X1,Y1,X2,Y2)),
  sixth_above(X1,Y1),
  terminal(X2,Y2).

build_line([X1,X2|Xs], [Y1,Y2|Ys]) :-
  note(Y2), 
  not(invalid_motion(X1,Y1,X2,Y2)),
  consonant(X1,Y1),
  consonant(X2,Y2),
  build_line([X2|Xs],[Y2|Ys]).

build_line([X1,X2], [Y1,Y2]):-
  note(Y1), note(Y2),
  not(invalid_motion(X1,Y1,X2,Y2)),
  sixth_above(X1,Y1),
  terminal(X2,Y2).

first_species([X1,X2|Xs], [Y1,Y2|Ys], true) :-
  note(Y1), note(Y2),
  terminal(X1,Y1),
  consonant(X1,Y1),
  consonant(X2,Y2),
  not(invalid_motion(X1,Y1,X2,Y2)), 
  build_random_line([X2|Xs], [Y2|Ys]).

first_species([X1,X2|Xs], [Y1,Y2|Ys], false) :-
  note(Y1), note(Y2),
  terminal(X1,Y1),
  consonant(X1,Y1),
  consonant(X2,Y2),
  not(invalid_motion(X1,Y1,X2,Y2)), 
  build_line([X2|Xs], [Y2|Ys]).

compose(Cf,Cp,Rand) :-
  maplist(note_number,Cf,Cf0),
  first_species(Cf0,Cp,Rand),
  maplist(name,Cp,Cp1),
  write(Cp1).

