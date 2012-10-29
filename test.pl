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
	cell(Sheet, X, Y, Value, _Type, Formula, _Style, _Annotation),
	Formula \== (-),
	format('Testing cell(~q,~q,~q) ~p~n', [Sheet,X,Y, Formula]),
	catch(ods_eval(Formula, OurValue),
	      E,
	      ( print_message(error, E),
		fail)),
	(   same_values(OurValue, Value)
	->  format('\tOK: cell(~q,~q,~q)~n', [Sheet,X,Y]),
	    fail
	;   format('\tWRONG: cell(~q,~q,~q) --> ~q [~q]~n', [Sheet, X, Y, OurValue, Value])
	).

same_values(X, Y) :-
	X == Y.
same_values(X, Y) :-
	number(X), number(Y),
	X =:= Y, !.
same_values(X, Y) :-
	number(X), number(Y),
	( float(X) ; float(Y) ), !,
	X/Y-1 < 0.000000001.



p(X,Y) :-
	sheet(Sheet),
	cell(Sheet, X, Y, Value, Type, Formula, Style, _Annotation),
	format('Value = ~q, Type = ~q, Formula = ~q, Style = ~q~n',
	       [ Value, Type, Formula, Style] ).
