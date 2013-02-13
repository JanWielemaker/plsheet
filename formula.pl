:- module(ods_formula,
	  [ sheet_formula_groups/3,	% :Sheet, -Groups, -Singles
	    generalize_formula/8,	% +S0, +X0, +Y0, +F0, -S, -X, -Y, -F
	    sheet_dependency_graph/2,	% :Sheet, -DepGraph
	    cell_dependency_graph/5	% :Sheet, +X, +Y, +Direction, -Graph
	  ]).
:- use_module(library(record)).
:- use_module(library(clpfd), except([transpose/2])).
:- use_module(library(ugraphs)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(ods_table).
:- use_module(datasource).

:- record
	map(sheet,x,y).

:- meta_predicate
	sheet_formula_groups(:, -),
	sheet_dependency_graph(:, -),
	cell_dependency_graph(:,+,+,+,-).

/** <module> Reason about formulas


*/

%%	sheet_formula_groups(:Sheet, -Groups, -Singles) is det.

sheet_formula_groups(Sheet, Groups, Singles) :-
	findall(f(Sheet,X,Y,F),
		cell_formula(Sheet, X, Y, F),
		Formulas),
	length(Formulas, Count),
	debug(formula, '~q: Found ~D formulas', [Sheet, Count]),
	map_list_to_pairs(skolem_formula, Formulas, Keyed),
	keysort(Keyed, Sorted),
	group_pairs_by_key(Sorted, ByKey),
	pairs_values(ByKey, CandidateGroups),
	length(CandidateGroups, CCount),
	debug(formula, '~D candidate groups', [CCount]),
	maplist(make_groups, CandidateGroups, NestedGroups, NestedSingles),
	append(NestedGroups, Groups),
	append(NestedSingles, Singles).

make_groups([], [], []).
make_groups([F0|FT], Groups, Singles) :-
	generalize_formula(F0, P),
	partition(match_formula(P), FT, Matching, Rest),
	length(Matching, Matches),
	length(Rest, Left),
	debug(formula, '~p: ~D matches; ~D left', [P, Matches, Left]),
	(   Matching \== []
	->  make_group(P, [F0|Matching], G0),
	    Groups = [G0|GT],
	    RS = Singles
	;   Groups = GT,
	    Singles = [F0|RS]
	),
	make_groups(Rest, GT, RS).

match_formula(P, F) :-
	\+ \+ P = F.

%%	make_group(+Pattern, +Matches, -Group)
%
%	Turn a set of matches into a group.  Groups is a term
%	forall(Var in Values, Formula).
%
%	@param Pattern is a term f(S,X,Y,F), where S,X,Y are variables.
%	@param Matches is a list of ground terms f(S,X,Y,F).

make_group(P, Matches, Groups) :-
	P = f(S,X,Y,_),
	findall(b(S,X,Y), member(P,Matches), Bindings),
	maplist(arg(1), Bindings, AllSheets), sort(AllSheets, Sheets),
	maplist(arg(2), Bindings, AllXs),     sort(AllXs, Xs),
	maplist(arg(3), Bindings, AllYs),     sort(AllYs, Ys),
	group(Sheets, Xs, Ys, P, Matches, Groups0),
	flatten(Groups0, Groups1),
	ds_formulas(Groups1, Groups, []).

group([S], [X], [Y], P, _, Result) :- !,
	P = f(S,X,Y,F),
	assertion(ground(P)),
	Result = (cell(S,X,Y) = F).
group([S], [X],  Ys, f(S,X,Y,F), _, forall(row,   Y in Set, f(S,X,Y,F))) :- !,
	compress(Ys, Set).
group([S], Xs,  [Y], f(S,X,Y,F), _, forall(col,   X in Set, f(S,X,Y,F))) :- !,
	compress(Xs, Set).
group(Ss,  [X], [Y], f(S,X,Y,F), _, forall(sheet, S in Ss,  f(S,X,Y,F))) :- !.
group([S], Xs, Ys, f(S,X,Y,F), Matches,
      [forall(area, [X in SetX, Y in SetY], f(S,X,Y,F))]) :-
	forall(( member(X,Xs),
		 member(Y,Ys)
	       ),
	       memberchk(f(S,X,Y,_), Matches)), !,
	compress(Xs, SetX),
	compress(Ys, SetY).
group([S], Xs, Ys, P, Matches, Groups) :-
	P = f(S,X,Y,_),
	length(Xs, Xc),
	length(Ys, Yc),
	(   Xc < Yc
	->  findall(G, (member(X,Xs), make_group(P, Matches, G)), NGroups)
	;   findall(G, (member(Y,Ys), make_group(P, Matches, G)), NGroups)
	),
	append(NGroups, Groups).

%%	compress(+List, -Description)
%
%	Create a short description of the elements in list using ranges.
%	Ranges are expressed as Low-High.

compress(List, Description) :-
	sort(List, Sorted),
	create_ranges(Sorted, Description).

create_ranges([], []).
create_ranges([Low|T0], [Low-High|T]) :-
	range(Low, T0, High, T1),
	High > Low, !,
	create_ranges(T1, T).
create_ranges([H|T0], [H|T]) :-
	create_ranges(T0, T).

range(Low, [Next|T0], High, T) :-
	succ(Low, Next), !,
	range(Next, T0, High, T).
range(High, T, High, T).


ds_formulas([], FL, FL).
ds_formulas([H|T], FL0, FL) :-
	ds_formula(H, FL0, FL1),
	ds_formulas(T, FL1, FL).

%%	ds_formula(+Group, -DSFormula, ?Tail) is det.
%
%	Translate a formula using the  forall()   notation  above into a
%	formula between data-sources. Some examples:
%
%	    * D1-D20 = A1-A20 + B1-B20

ds_formula(forall(_, _ in [], _), FL, FL) :- !.
					% rows
ds_formula(forall(row, Y in [Ya-Yz|T], P),
	   [cell_range(S,X,Ya,X,Yz) = FDS|More], FL) :- !,
	P = f(S,X,Y,F),
	range_formula(y(Y,Ya,Yz), F, FDS),
	assertion(ground(FDS)),
	ds_formula(forall(row, Y in T, P), More, FL).
ds_formula(forall(row, Y in [Y0|Ys], P),
	   [cell(S,X,Y0) = FDS|More], FL) :- !,
	P = f(S,X,Y,F),
	range_formula(y(Y,Y0,Y0), F, FDS),
	assertion(ground(FDS)),
	ds_formula(forall(row, Y in Ys, P), More, FL).
					% columns
ds_formula(forall(col, X in [Xa-Xz|T], P),
	   [cell_range(S,Xa,Y,Xz,Y) = FDS|More], FL) :- !,
	P = f(S,X,Y,F),
	range_formula(x(X,Xa,Xz), F, FDS),
	assertion(ground(FDS)),
	ds_formula(forall(col, X in T, P), More, FL).
ds_formula(forall(col, X in [X0|Xs], P),
	   [cell(S,X0,Y) = FDS|More], FL) :- !,
	P = f(S,X,Y,F),
	range_formula(x(X,X0,X0), F, FDS),
	assertion(ground(FDS)),
	ds_formula(forall(col, X in Xs, P), More, FL).
					% areas
ds_formula(forall(area, [_ in [],_], _), FL, FL) :- !.
ds_formula(forall(area, [_,_ in []], _), FL, FL) :- !.
ds_formula(forall(area, [X in [Xa-Xz|TX], Y in [Ya-Yz|TY]], P),
	   [ cell_range(S,Xa,Ya,Xz,Yz) = FDS | More ], FL) :- !,
	P = f(S,X,Y,F),
	range_formula(xy(X,Xa,Xz,Y,Ya,Yz), F, FDS),
	assertion(ground(FDS)),
	ds_formula(forall(area, [X in TX, Y in [Ya-Yz|TY]], P),
		   More, FL0),
	ds_formula(forall(area, [X in [Xa-Xz|TX], Y in TY], P),
		   FL0, FL).

ds_formula(Formula, [Formula|FL], FL).		% TBD


%%	range_formula(+Spec, +F, -FDS)

					% y...
range_formula(y(Y,Ya,Ya), cell(S,X,YF), cell(S,X,Ys)) :-
	findall(YF, Y=Ya, [Ys]), !.
range_formula(y(Y,Ya,Ya), cell_range(S,Xs,YFs,Xe,YFe),
	                  cell_range(S,Xs,Ys,Xe,Ye)) :-
	findall(YFs-YFe, Y=Ya, [Ys-Ye]), !.
range_formula(y(Y,Ya,Yz), cell(S,X,YF), cell_range(S,X,Ys,X,Ye)) :-
	findall(YF, (Y=Ya; Y=Yz), [Ys,Ye]), !.
					% x...
range_formula(x(X,Xa,Xa), cell(S,XF,Y), cell(S,Xs,Y)) :-
	findall(XF, X=Xa, [Xs]), !.
range_formula(x(X,Xa,Xa), cell_range(S,XFs,Ys,XFe,Ye),
	                  cell_range(S,Xs,Ys,Xe,Ye)) :-
	findall(XFs-XFe, X=Xa, [Xs-Xe]), !.
range_formula(x(X,Xa,Xz), cell(S,XF,Y), cell_range(S,Xs,Y,Xe,Y)) :-
	findall(XF, (X=Xa; X=Xz), [Xs,Xe]), !.
					% xy...
range_formula(xy(X,Xa,Xz,Y,Ya,Yz),
	      cell(S,XF,YF),
	      cell_range(S,Xs,Ys,Xe,Ye)) :-
	findall(XF, (X=Xa; X=Xz), [Xs,Xe]),
	findall(YF, (Y=Ya; Y=Yz), [Ys,Ye]), !.
					% General recursion
range_formula(Y, From, To) :-
	compound(From), !,
	From =.. [Name|Args0],
	maplist(range_formula(Y), Args0, Args),
	To =.. [Name|Args].
range_formula(_, Formula, Formula).



%%	generalize_formula(F0, F) is det.
%%	generalize_formula(+S0, +X0, +Y0, +F0, -S, -X, -Y, -F) is det.
%
%	F is F0, after replacing coordinates by the variables X and Y or
%	constraints thereof. The idea is  that   F  now unifies to other
%	formulas that have the same  structure   with  the same relative
%	cell positions.

generalize_formula(f(S0,X0,Y0,F0), f(S,X,Y,F)) :-
	generalize_formula(S0, X0, Y0, F0, S, X, Y, F).

generalize_formula(S0, X0, Y0, F0, S, X, Y, F) :-
	Map = map(S0-S, X0-X, Y0-Y),
	generalize_formula(Map, F0, F).

skolem_formula(f(S0,X0,Y0,F0), F) :-
	generalize_formula(S0, X0, Y0, F0, 'S', 'X', 'Y', F).



generalize_formula(Map, cell(S0,X0,Y0), cell(S,X,Y)) :- !,
	generalize_sheet(S0, Map, S),
	generalize_x(X0, Map, X),
	generalize_y(Y0, Map, Y).
generalize_formula(Map,
		   cell_range(S0,SX0,SY0,EX0,EY0),
		   cell_range(S, SX, SY, EX, EY)) :- !,
	generalize_sheet(S0, Map, S),
	generalize_x(SX0, Map, SX),
	generalize_y(SY0, Map, SY),
	generalize_x(EX0, Map, EX),
	generalize_y(EY0, Map, EY).
generalize_formula(Map, From, To) :-
	compound(From), !,
	From =.. [Name|Args0],
	maplist(generalize_formula(Map), Args0, Args),
	To =.. [Name|Args].
generalize_formula(_, Formula, Formula).


generalize_sheet(S0, Map, S) :-
	map_sheet(Map, F-T),
	(   S0 == F
	->  S = T
	;   S = S0
	).
generalize_x(X0, Map, X) :-
	map_x(Map, F-T),
	generalize_cordinate(X0, F-T, X).
generalize_y(Y0, Map, Y) :-
	map_y(Map, F-T),
	generalize_cordinate(Y0, F-T, Y).

generalize_cordinate(X0, F-T, X) :-
	(   X0 == F
	->  X = T
	;   atom(T)
	->  X = T
	;   integer(X0)
	->  Dif is X0-F,
	    (	Dif > 0
	    ->	X #= T+Dif
	    ;	MinDif is -Dif,
		X #= T-MinDif
	    )
	;   X = X0
	).

%%	sheet_dependency_graph(:Sheet, -UGraph) is det.
%
%	Create a UGraph that represents  the dependencies between cells.
%	Nodes in the cells are terms cell(S,X,Y).

sheet_dependency_graph(Sheet, Graph) :-
	findall(Cell-Dep, cell_dependency(Sheet, Cell, Dep), Graph0),
	sort(Graph0, Graph1),
					% Add missing (source) nodes
	pairs_keys_values(Graph1, Left, RightSets),
	append(RightSets, Right0),
	sort(Right0, Right),
	ord_subtract(Right, Left, Sources),
	maplist(pair_nil, Sources, SourceTerms),
	ord_union(Graph1, SourceTerms, Graph2),
	transpose(Graph2, Graph).

pair_nil(X, X-[]).

cell_dependency(Sheet, cell(Sheet,X,Y), Inputs) :-
	Sheet = M:_,
	cell_formula(Sheet, X, Y, Formula),
	formula_cells(Formula, M, Inputs0, []),
	sort(Inputs0, Inputs).

formula_cells(cell(S,X,Y), M, [cell(M:S,X,Y)|T], T) :- !.
formula_cells(DataSource, M,  Cells, Rest) :-
	DataSource = cell_range(S,SX,SY,EX,EY), !,
	debug(dep, 'DataSource: ~q', [DataSource]),
	(   forall(ds_inside(DataSource,X,Y),
		   \+ cell_formula(M:S,X,Y,_))
	->  debug(dep, 'DataSource without formulas: ~p', [DataSource]),
	    Cells = [cell_range(M:S,SX,SY,EX,EY)|Rest]
	;   findall(cell(M:S,X,Y), ds_inside(DataSource,X,Y), Cells, Rest)
	).
formula_cells(ext(URL, DS), _M, Cells, Cells) :- !,
	debug(dep, 'External ref: ~p ~p', [URL, DS]).
formula_cells(Compound, M, Cells, Rest) :-
	compound(Compound), !,
	Compound =.. [_|Args],
	list_formula_cells(Args, M, Cells, Rest).
formula_cells(_, _, Cells, Cells).

list_formula_cells([], _, Cells, Cells).
list_formula_cells([H|T], M, Cells, Rest) :-
	formula_cells(H, M, Cells, Rest0),
	list_formula_cells(T, M, Rest0, Rest).

%%	cell_dependency_graph(:Sheet, +X, +Y, +Direction, -Graph) is det.
%
%	True when Graph is an  Ugraph   expressing  the  dependencies of
%	StartCell. Direction is one of =inputs=, =outputs= or =both=.
%
%	@tbd	Implement =outputs= and =both=. Probably need to
%		materialize the dependecies for that.  We could do
%		that while loading the spreadsheet?

cell_dependency_graph(Sheet, X, Y, inputs, Graph) :- !,
	input_graph(Sheet, X, Y, Graph).
cell_dependency_graph(_,_,_,Dir,_) :-
	must_be(oneof([inputs]), Dir).

input_graph(Sheet, Col, Y, Graph) :-
	column_x(Col, X),
	Cell0 = cell(Sheet,X,Y),
	empty_assoc(V0),
	put_assoc(Cell0, V0, true, V1),
	traverse_input_graph([Cell0], V1, Edges, []),
	vertices_edges_to_ugraph([Cell0], Edges, Graph).

traverse_input_graph([], _, Edges, Edges).
traverse_input_graph([Cell0|CellT], Visited0, Edges, ETail) :-
	inputs(Cell0, Inputs),
	edges(Inputs, Cell0, Edges, Tail0),
	update_visited(Inputs, Visited0, Visited1, NewInputs, CellT),
	traverse_input_graph(NewInputs, Visited1, Tail0, ETail).

inputs(cell(Sheet,X,Y), Inputs) :-
	cell_formula(Sheet, X, Y, Formula), !,
	Sheet = M:_,
	formula_cells(Formula, M, Inputs, []).
inputs(_, []).

edges([], _, Edges, Edges).
edges([H|T], V0, [H-V0|Edges], ET) :-
	edges(T, V0, Edges, ET).

update_visited([], Visited, Visited, Inputs, Inputs).
update_visited([H|T], Visited0, Visited, Inputs0, Inputs) :-
	get_assoc(H, Visited0, _), !,
	update_visited(T, Visited0, Visited, Inputs0, Inputs).
update_visited([H|T], Visited0, Visited, [H|Inputs1], Inputs) :-
	put_assoc(H, Visited0, true, Visited1),
	update_visited(T, Visited1, Visited, Inputs1, Inputs).


column_x(Col, X) :-
	atom(Col), !,
	upcase_atom(Col, COL),
	column_name(X, COL).
column_x(Col, X) :-
	integer(Col), !,
	X = Col.
column_x(Col, _) :-
	type_error(column, Col).
