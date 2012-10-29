:- use_module(ods_table).

file('E-Design WindEnergie.ods').
sheet('WindopLand').

dom(DOM) :-
	file(File),
	ods_DOM(File, DOM, []).

dom(Member, DOM) :-
	file(File),
	ods_DOM(File, DOM, [member(Member)]).

load :-
	file(File),
	load(File).

load(File) :-
	ods_clean,
	ods_load(File).

eval(Sheet, X,Y) :-
	eval(Sheet, X,Y, fail).

eval(Sheet, X,Y, Cont) :-
	forall(cell(Sheet, X, Y, Value, _Type, Formula, _Style, _Annotation),
	       (   Formula == (-)
	       ->  true
	       ;   format('Testing ~p ~p [OK: ~q]~n',
			  [cell(Sheet,X,Y), Formula, Value]),
		   catch(ods_eval(Formula, OurValue),
			 E,
			 ( print_message(error, E),
			   Cont)),
		   (   var(E)
		   ->  (   same_values(OurValue, Value)
		       ->  format('\tOK: ~p~n', [cell(Sheet,X,Y)])
		       ;   format('\tWRONG: ~p --> ~q [OK: ~q]~n',
				  [cell(Sheet,X,Y), OurValue, Value]),
			   Cont
		       )
		   ;   true
		   )
	       )).

same_values(X, Y) :-
	X == Y.
same_values(X, Y) :-
	number(X), number(Y),
	X =:= Y, !.
same_values(X, Y) :-
	number(X), number(Y),
	( float(X) ; float(Y) ), !,
	(   Y =\= 0
	->  X/Y-1 < 0.000000001
	;   abs(X-Y) < 0.000000001
	).



p(X,Y) :-
	sheet(Sheet),
	cell(Sheet, X, Y, Value, Type, Formula, Style, _Annotation),
	format('Value = ~q, Type = ~q, Formula = ~q, Style = ~q~n',
	       [ Value, Type, Formula, Style] ).

%%	col_name(+N, -Name) is det.
%
%	Name is the alplanumerical name of column  Col. Column 1 is 'A',
%	26 = 'Z', 27 = 'AA'.

col_name(N, Row) :-
	col_chars(N, Chars, []),
	atom_codes(Row, Chars).

col_chars(R, [C|T], T) :-
	R =< 26, !,
	C is R+0'A-1.
col_chars(R, List, T) :-
	High is R//26,
	Last is (R mod 26) + 0'A - 1,
	col_chars(High, List, [Last|T]).

user:portray(cell(Sheet,X,Y)) :-
	col_name(X, C),
	format('~w.~w~w', [Sheet, C, Y]).
user:portray(cell_range(Sheet,SX,SY,EX,EY)) :-
	col_name(SX, CS),
	col_name(EX, CE),
	format('[~w.~w~w:~w~w]', [Sheet, CS,SY,CE,EY]).
