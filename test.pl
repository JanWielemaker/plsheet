:- use_module(ods_table).

file('E-Design WindEnergie.ods').
sheet('WindopLand').

dom(DOM) :-
	file(File),
	ods_DOM(File, DOM, []).

load :-
	file(File),
	ods_clean,
	ods_load(File).

eval(Sheet, X,Y) :-
	cell(Sheet, X, Y, Value, _Type, Formula, _Style, _Annotation),
	Formula \== (-),
	format('Eval cell(~q,~q) ~p~n', [X, Y, Formula]),
	catch(ods_eval(Formula, OurValue),
	      E,
	      ( print_message(error, E),
		fail)),
	(   same_values(OurValue, Value)
	->  format('OK: cell(~w,~w)~n', [X,Y]),
	    fail
	;   format('cell(~w,~w) --> ~q [~q]~n', [X, Y, OurValue, Value])
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
