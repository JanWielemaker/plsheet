:- use_module(ods_table).
:- use_module(recognise).
:- use_module(datasource).
:- use_module(table).
:- use_module(data).
:- use_module(labels).
:- use_module(formula).
:- use_module(varnames).
:- use_module(webui).
:- use_module(library(debug)).
:- use_module(library(graphml_ugraph)).

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

file('E-Design-v2.205 sept 2011.ods').
sheet('WindopLand').

dom(DOM) :-
	file(File),
	ods_DOM(File, DOM, []).

dom(Member, DOM) :-
	file(File),
	ods_DOM(File, DOM, [member(Member)]).

load :-
	file(File),
	load(File),
	consult('ds_formulas.db'),
	ds_graph.

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
	assert_blocks(_Sheet1,_Type),
	color_sheets(_Sheet2, block),
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
		 *	   FORMULA-GRAPH	*
		 *******************************/

:- meta_predicate
	formula_graph(:, +).

formula_graph(Sheet, File) :-
	sheet_dependency_graph(Sheet, UGraph),
	cell_graph(UGraph, File).

cell_graph(UGraph, File) :-
	graphml_write_ugraph(File, map_cell,
			     [ key(node, 'Label',  string),
			       key(node, sheet,    string),
			       key(node, workbook, string),
			       key(node, row,	   int),
			       key(node, column,   int),
			       key(node, r,        int),
			       key(node, g,        int),
			       key(node, b,        int)
			     ],
			     UGraph).

map_cell(Field, node(Node), Value) :-
	map_node(Field, Node, Value).

map_node(id, Node, Id) :- fail,
	with_output_to(atom(Id), print(Node)).
map_node('Label', cell(_, X, Y), Label) :-
	column_print_name(X, C),
	row_print_name(Y, R),
	format(atom(Label), '~w~w', [C, R]).
map_node(sheet, cell(_:S,_X,_Y), S).
map_node(workbook, cell(M:_,_X,_Y), M).
map_node(column, cell(_,X,_), X).
map_node(row, cell(_,_,Y), Y).
map_node(r, cell(_:S,_,_), R) :-
	assert_seen(S),
	sheet_color(S, Color),
	color_rgb(Color, R,_,_).
map_node(g, cell(_:S,_,_), G) :-
	sheet_color(S, Color),
	color_rgb(Color, _,G,_).
map_node(b, cell(_:S,_,_), B) :-
	sheet_color(S, Color),
	color_rgb(Color, _,_,B).

:- dynamic seen/1.

assert_seen(S) :-
	seen(S), !.
assert_seen(S) :-
	assertz(seen(S)).

color_rgb(Name, R, G, B) :-
	new(C, colour(Name)),
	get(C, red, R0), R is R0//256,
	get(C, green, G0), G is G0//256,
	get(C, blue, B0), B is B0//256.

sheet_color('Macro',			    gray50).
sheet_color('Parameters',		    green).
sheet_color('Project',			    gray50).
sheet_color('Technologiekeuzen',	    blue).
sheet_color('Resultaten GUI',		    red2).
sheet_color('Stadsverkeer',		    salmon1).
sheet_color('Personenauto',		    salmon2).
sheet_color('Vrachtverkeer',		    salmon3).
sheet_color('GO WB',			    goldenrod1).
sheet_color('GO WN',			    goldenrod2).
sheet_color('GO Ut',			    goldenrod3).
sheet_color('Bedrijven Warmte',		    salmon4).
sheet_color('Chemie',			    lightsalmon1).
sheet_color('Kunstmest',		    lightsalmon2).
sheet_color('Staal',			    lightsalmon3).
sheet_color('Landbouw',			    lightsalmon4).
sheet_color('Elektriciteitsproductie',	    gray50).
sheet_color('Elektriciteitsopslag',	    gray50).
sheet_color('Elektriciteitsbalans',	    gray50).
sheet_color('Elektriciteit productie uren', gray50).
sheet_color('Vloeibare brandstoffen',	    gray50).
sheet_color('Methaan',			    gray50).
sheet_color('Waterstof',		    gray50).
sheet_color('Diverse gegevens',		    green4).
sheet_color('Voorraden',		    gray50).
sheet_color('Infrastructuur',		    gray50).
sheet_color('Resultaatoverzicht',	    red3).
sheet_color('Dataset 1',		    yellow1).
sheet_color('Dataset 2',		    yellow2).
sheet_color('Dataset 3',		    yellow3).
sheet_color('Doelen',			    red4).

:- dynamic ds_formula/2.
:- dynamic ds_input/2.

ds_formulas :-
	retractall(ds_formula(_,_)),
	forall(sheet(S, _),
	       ( format(user_error, 'Processing ~q ... ', [S]),
		 time(sheet_ds_formulas(S, Formulas)),
		 length(Formulas, Count),
		 format(user_error, 'found ~D~n', [Count]),
		 maplist(assert_formula, Formulas)
	       )).

assert_formula(Target = Formula) :-
	assert(ds_formula(Target, Formula)).

ds_graph :-
	retractall(ds_input(_,_)),
	forall(( ds_formula(Target, Formula),
		 ds_member(TDS, Formula)
	       ),
	       assert(ds_input(Target, TDS))).

ds_member(M, Formula) :-
	sub_term(M, Formula),
	compound(M),
	(   M = cell(_,_,_)
	->  true
	;   M = cell_range(_,_,_,_,_)
	).

ds_set(Sheet, Set) :-
	findall(DS, ds(Sheet, DS), All),
	sort(All, Set).

ds(Sheet, DS) :-
	ds_input(T,I),
	(   to_ds(Sheet, T, DS)
	;   to_ds(Sheet, I, DS)
	).

to_ds(S, cell(S,X,Y), cell_range(S,X,Y,X,Y)) :- !.
to_ds(S, Range, Range) :-
	arg(1, Range, S).

:- use_module(kmeans).
:- use_module(rtree).

ds_clusters(Sheet, Count, Clusters) :-
	ds_set(Sheet, Set),
	k_means(ds_rect, Count, Set, Clusters).

ds_rect(cell_range(_,Xs,Ys,Xe,Ye),
	rect(Xs,Ys,Xe,Ye)).

ds_tree(Sheet, Tree) :-
	ds_set(Sheet, Set),
	list_to_rtree(ds_rect, Set, Tree).

ds_intersections(Sheet, DS1, DS2) :-
	ds_set(Sheet, Set),
	list_to_rtree(ds_rect, Set, Tree),
	member(DS1, Set),
	rtree_intersects(Tree, DS1, DS2),
	DS1 \== DS2.

ds_intersections_1(Sheet, DS1, DS2) :-
	ds_set(Sheet, Set),
	list_to_rtree_inc(ds_rect, Set, Tree),
	member(DS1, Set),
	rtree_intersects(Tree, DS1, DS2),
	DS1 \== DS2.

list_to_rtree_inc(Map, List, Tree) :-
	empty_rtree(Map, Tree0),
	rtree_insert_list(List, Tree0, Tree).

rtree_insert_list([], Tree, Tree).
rtree_insert_list([H|T], Tree0, Tree) :-
	(   rtree_insert(H, Tree0, Tree1)
	->  true
	;   gtrace,
	    rtree_insert(H, Tree0, Tree1)
	),
	put_char(user_error, '.'),
	rtree_insert_list(T, Tree1, Tree).

ds_intersections_2(Sheet, DS1, DS2) :-
	ds_set(Sheet, Set),
	member(DS1, Set),
	member(DS2, Set),
	DS1 \== DS2,
	ds_intersection(DS1, DS2, _).


		 /*******************************
		 *	      FEEDBACK		*
		 *******************************/

println(X) :-
	write_term(X, [ portray(true)/*,
			attributes(portray)*/
		      ]),
	nl.

:- multifile
	user:portray/3,
	user:message_property/2.

user:portray(cell(Sheet,X,Y)) :-
	column_print_name(X, C),
	row_print_name(Y, R),
	format('~q.~w~w', [Sheet, C, R]).
user:portray(cell_range(Sheet,SX,SY,EX,EY)) :-
	column_print_name(SX, SC),
	column_print_name(EX, EC),
	row_print_name(SY, SR),
	row_print_name(EY, ER),
	(   atom(Sheet),
	    \+ sheet_name_need_quotes(Sheet)
	->  format('[~w.~w~w:~w~w]', [Sheet, SC,SR,EC,ER])
	;   format('[\'~w\'.~w~w:~w~w]', [Sheet, SC,SR,EC,ER])
	).

column_print_name(X, C) :-
	variable_name(X, C), !.
column_print_name(X, C) :-
	integer(X), !,
	column_name(X, C).
column_print_name(X, X).

row_print_name(Y, R) :-
	variable_name(Y, R), !.
row_print_name(Y, Y).


user:message_property(debug(ods(test(ok))), color(fg(green))).
user:message_property(debug(ods(test(error))), color(fg(red))).
user:message_property(debug(ods(test(wrong))), color(fg(red))).
user:message_property(debug(ods(test(failed))), color(fg(red))).
