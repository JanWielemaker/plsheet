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

test(Sheet, X,Y) :-
	test(Sheet, X,Y, fail).

test(Sheet, X,Y, Cont) :-
	State = state(0,_),
	(   forall(cell_formula(Sheet,X,Y,Formula),
		   (   verify(Sheet,X,Y,Formula)
		   ->  arg(1, State, P0),
		       P is P0+1,
		       nb_setarg(1, State, P)
		   ;   Cont
		   ))
	->  true
	;   arg(1, State, Passed),
	    format('Passed: ~D~n', [Passed]),
	    fail
	).


verify(Sheet,X,Y,Formula) :-
	cell_value(Sheet, X, Y, Value),
	format('Testing test(~q,~q,~q) ~p [OK: ~q]~n',
	       [Sheet,X,Y, Formula, Value]),
	(   catch(cell_eval(Sheet, X, Y, OurValue),
		  E,
		  ( message_to_string(E, Msg),
		    format('\tERROR: ~p [OK: ~q]: ~w~n',
			   [cell(Sheet,X,Y), Value, Msg])
		  ))
	->  var(E),
	    (	same_values(OurValue, Value)
	    ->  format('\tOK: ~p~n', [cell(Sheet,X,Y)])
	    ;   format('\tWRONG: ~p --> ~q [OK: ~q]~n',
		       [cell(Sheet,X,Y), OurValue, Value]),
		fail
	    )
	;   format('\tFAILED: ~p [OK: ~q]~n',
		   [cell(Sheet,X,Y), Value]),
	    fail
	).

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

user:portray(cell(Sheet,X,Y)) :-
	column_name(X, C),
	format('~w.~w~w', [Sheet, C, Y]).
user:portray(cell_range(Sheet,SX,SY,EX,EY)) :-
	column_name(SX, CS),
	column_name(EX, CE),
	format('[~w.~w~w:~w~w]', [Sheet, CS,SY,CE,EY]).
