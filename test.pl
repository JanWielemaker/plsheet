:- use_module(ods_table).
:- use_module(recognise).
:- use_module(table).
:- use_module(data).
:- use_module(labels).
:- use_module(webui).
:- use_module(library(debug)).

:- initialization
	start_server.

:- dynamic
	server_url/1.

start_server :-
	server_url(_), !.
start_server :-
	server(Port),
	format(atom(URL), 'http://localhost:~d/', [Port]),
	assertz(server_url(URL)),
	www_open_url(URL).

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
	ods_unload,
	ods_load(File).


		 /*******************************
		 *	     SEGMENTING		*
		 *******************************/

%%	segment
%
%	Segment the tables.  First try.  To show the result, run
%
%	   ==
%	   ?- sheet(S,_), clear, show(sheet(S)).
%	   ==

segment :-
	clean_data,
	assert_labels(_Sheet),
	assert_tables(_Sheet1,_Type),
	color_tables(_Sheet2),
	true.




		 /*******************************
		 *	 FORMULA TESTING	*
		 *******************************/

test(Sheet, X,Y) :-
	test(Sheet, X,Y, fail).

:- dynamic
	passed/3,
	failed/4.

:- meta_predicate
	test(:, ?, ?, 0).

test(Sheet, X,Y, Cont) :-
	clean_stats,
	State = state(0),
	(   forall(cell_formula(Sheet,X,Y,Formula),
		   ( (   verify(Sheet,X,Y,Formula)
		     ->  assertz(passed(Sheet,X,Y))
		     ;   Cont
		     ),
		     step(State)
		   ))
	->  stats
	;   stats,
	    fail
	).

step(State) :-
	arg(1, State, T0),
	T is T0 + 1,
	nb_setarg(1, State, T),
	(   T mod 1000 =:= 0
	->  format(user_error, '\r~t~D~20|', [T])
	;   true
	).

clean_stats :-
	retractall(passed(_,_,_)),
	retractall(failed(_,_,_,_)).

stats :-
	predicate_property(passed(_,_,_), number_of_clauses(Passed)),
	predicate_property(failed(_,_,_,_), number_of_clauses(Failed)),
	format('~NPassed: ~D, failed: ~D~n', [Passed,Failed]).


verify(Sheet,X,Y,Formula) :-
	cell_value(Sheet, X, Y, Value),
	debug(ods(test),
	      'Testing test(~q,~q,~q) ~p [OK: ~q]',
	      [Sheet,X,Y, Formula, Value]),
	(   catch(cell_eval(Sheet, X, Y, OurValue),
		  E,
		  ( message_to_string(E, Msg),
		    assertz(failed(Sheet,X,Y,error(E, Msg))),
		    debug(ods(test(error)),
			  '\tERROR: ~p [OK: ~q]: ~w',
			   [cell(Sheet,X,Y), Value, Msg])
		  ))
	->  var(E),
	    (	same_values(OurValue, Value)
	    ->  debug(ods(test(ok)), '\tOK: ~p', [cell(Sheet,X,Y)])
	    ;   assertz(failed(Sheet,X,Y,wrong(OurValue,Value))),
	        debug(ods(test(wrong)),
		      '\tWRONG: ~p --> ~q [OK: ~q]',
		      [cell(Sheet,X,Y), OurValue, Value]),
		fail
	    )
	;   assertz(failed(Sheet,X,Y,failed)),
	    debug(ods(test(failed)),
		  '\tFAILED: ~p [OK: ~q]',
		  [cell(Sheet,X,Y), Value]),
	    fail
	).

%%	same_values(+OurValue, +Value)

same_values(X, Y) :-
	X == Y.
same_values(' ', '').			% Hack.  test('Dataset 3', 23, 79).
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

:- meta_predicate
	p(:,+,+).

p(M:Sheet, X,Y) :-
	ods_table:cell_id(X, Y, Id),
	M:cell(Sheet, Id, Value, Type, Formula, Style, _Annotation),
	format('Value = ~q, Type = ~q, Formula = ~q, Style = ~q',
	       [ Value, Type, Formula, Style] ).


		 /*******************************
		 *	      FEEDBACK		*
		 *******************************/

:- multifile
	user:portray/3,
	user:message_property/2.

user:portray(cell(Sheet,X,Y)) :-
	integer(X),
	column_name(X, C),
	format('~q.~w~w', [Sheet, C, Y]).
user:portray(cell_range(Sheet,SX,SY,EX,EY)) :-
	integer(SX), integer(EX),
	column_name(SX, CS),
	column_name(EX, CE),
	(   atom(Sheet),
	    \+ sheet_name_need_quotes(Sheet)
	->  format('[~w.~w~w:~w~w]', [Sheet, CS,SY,CE,EY])
	;   format('[\'~w\'.~w~w:~w~w]', [Sheet, CS,SY,CE,EY])
	).

user:message_property(debug(ods(test(ok))), color(fg(green))).
user:message_property(debug(ods(test(error))), color(fg(red))).
user:message_property(debug(ods(test(wrong))), color(fg(red))).
user:message_property(debug(ods(test(failed))), color(fg(red))).
