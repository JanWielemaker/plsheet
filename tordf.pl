:- module(tordf,
	  [ assert_cells/2			% :Sheet, +Graph
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(ods_table).
:- use_module(datasource).
:- use_module(recognise).

:- rdf_register_prefix(d2s, 'http://www.data2semantics.org/core/').
:- rdf_register_prefix(table, 'http://www.data.org/').

:- meta_predicate
	assert_cells(:, +),
	cell_url(:, +, +, -).

assert_cells(Sheet, Graph) :-
	sheet_bb(Sheet, cell_range(_,SX,SY,EX,EY)),
	SY1 is SY+1,
	SX1 is SX+1,
	forall(between(SY1, EY, Y),
	       forall(between(SX1, EX, X),
		      assert_lt(Sheet, X, Y, Graph))),
	forall(between(SY, EY, Y),
	       forall(between(SX, EX, X),
		      assert_cell(Sheet, X, Y, SX, SY, Graph))).

assert_lt(Sheet, X, Y, Graph) :-
	Left is X-1,
	Above is Y-1,
	cell_url(Sheet, X, Y, MeURL),
	cell_url(Sheet, Left, Y, LeftURL),
	cell_url(Sheet, X, Above, AboveURL),
	rdf_assert(MeURL, d2s:rightOf, LeftURL, Graph),
	rdf_assert(MeURL, d2s:below, AboveURL, Graph).

assert_cell(Sheet, X, Y, SX, SY, Graph) :-
	cell_class(Sheet, X, Y, Class),
	cell_url(Sheet, X, Y, URL),
	rdf_global_id(d2s:Class, Type),
	rdf_assert(URL, rdf:type, Type, Graph),
	(   Class \== empty,
	    cell_value(Sheet, X, Y, Value)
	->  value_literal(Class, Value, Literal),
	    rdf_assert(URL, d2s:value, Literal, Graph)
	;   true
	),
	possible_label(Class, Sheet, X, Y, SX, SY, Graph), !.
assert_cell(Sheet, X, Y, SX, SY, Graph) :-
	gtrace,
	assert_cell(Sheet, X, Y, SX, SY, Graph).

value_literal(float, Num, literal(type(Type, Num))) :-
	rdf_equal(Type, xsd:double).
value_literal(percentage, Num, literal(type(Type, Num))) :-
	rdf_equal(Type, xsd:percentage).
value_literal(string, String, literal(String)).

possible_label(Class, Sheet, X, Y, SX, SY, Graph) :-
	(   data_cell(Class),
	    cell_url(Sheet, X, Y, MeURL),
	    (	between(SX, X, LX),
		cell_class(Sheet, LX, Y, string),
		cell_url(Sheet, LX, Y, LabelURL),
		rdf_assert(MeURL, d2s:leftLabelCandidate, LabelURL, Graph)
	    ;	between(SY, Y, LY),
		cell_class(Sheet, X, LY, string),
		cell_url(Sheet, X, LY, LabelURL),
		rdf_assert(MeURL, d2s:topLabelCandidate, LabelURL, Graph)
	    ),
	    fail
	;   true
	).

data_cell(float).
data_cell(percentage).

cell_url(Sheet, X, Y, URL) :-
	column_name(X, Col),
	format(atom(Local), '~w/~w~d', [Sheet, Col, Y]),
	rdf_global_id(table:Local, URL).
