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

eval(X,Y) :-
	sheet(Sheet),
	cell(Sheet, X, Y, Value, _Type, Formula, _Style, _Annotation),
	Formula \== (-),
	format('Eval ~p~n', [Formula]),
	catch(ods_eval(Formula, OurValue),
	      E,
	      ( print_message(error, E),
		fail)),
	format('~w,~w --> ~q [~q]~n', [X, Y, OurValue, Value]).

p(X,Y) :-
	sheet(Sheet),
	cell(Sheet, X, Y, Value, Type, Formula, Style, _Annotation),
	format('Value = ~q, Type = ~q, Formula = ~q, Style = ~q~n',
	       [ Value, Type, Formula, Style] ).
