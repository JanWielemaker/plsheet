:- module(table,
	  [ assert_tables/2,		% ?Sheet, ?Type
	    data_blocks/3,		% :Sheet, ?Type, ?Blocks
	    assert_blocks/2,		% ?Sheet, ?Type
	    block_union_new_non_empty/3,% +Blocks, -Union, -NewNonEmpty
	    tables/3,			% ?Sheet, +Type, -Tables
	    table/2,			% +Data, -Support

	    adjacent_objects/5,		% :Sheet, +Type, ?Obj1, ?Obj2, ?Rel
	    intersecting_objects/5,	% :Sheet, +Type, ?Tab1, ?Tab2, -Intersection
	    color_sheets/2,		% :Sheet, ?What

	    cells_outside_tables/3	% +Sheet, +Table, -Cells
	  ]).
:- use_module(recognise).
:- use_module(datasource).
:- use_module(ods_table).
:- use_module(data).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(clpfd), except([transpose/2])).
:- use_module(library(ugraphs)).

:- meta_predicate
	tables(:, ?, -),
	assert_tables(:, ?),
	data_blocks(:, +, -),
	assert_blocks(:, ?),
	adjacent_objects(:, +, ?, ?, ?),
	intersecting_objects(:, +, ?, ?, ?),
	color_sheets(:, ?).

/** <module> Detect tables
*/

		 /*******************************
		 *	       TABLES		*
		 *******************************/

%%	assert_tables(:Sheet, ?Type) is det.
%
%	Infer and assert identified tables. Creates the following facts:
%
%	  * table(TableID, Type, MainDS, HeaderDSList, UnionDS)
%	  * cell_property(Sheet, X, Y, table(TableID))

assert_tables(Sheet, Type) :-
	Sheet = M:_,
	tables(Sheet, Type, Tables),
	forall(member(T, Tables),
	       assert_table(M:T)),
	(   Tables == []
	->  true
	;   assert_tables(Sheet, Type)
	).

%%	tables(?Sheet, +Type, -Tables) is det.
%
%	Make an initial guess  at  all  tables.   Table  is  a  list  of
%	table(Data, Headers,Union).

tables(Sheet, Type, Tables) :-
	findall(SheetTables,
		( setof(Table,
			Type^table_in_sheet(Sheet, Type, Table),
			SheetTables0),
		  remove_inside(SheetTables0, SheetTables)
		),
		NestedTables),
	append(NestedTables, Tables).

table_in_sheet(M:Sheet, Type, table(Id,Type,DS,Headers,Union)) :-
	ds_sheet(DS, Sheet),
	cell_class(Type),
	unassigned_anchor(DS, Type),
	once((block(M:DS, Type),
	      table(M:DS, Headers))),
	ds_union([DS|Headers], Union),
	ds_id(DS, Id, table).


		 /*******************************
		 *	    SUPER BLOCKS	*
		 *******************************/

%%	data_blocks(:Sheet, +Type, -Blocks) is nondet.
%
%	True when Blocks is a list   of  non-verlapping datasources that
%	contains all detected blocks.  This implies that we need to
%
%	  - resolve intersections.  There are several options:
%	    - create a union from the intersecting blocks
%	    - Split into independent rectangles
%	  - Optionally join adjacent
%
%	@param Type is the cell-type (=string=, =float=, ...)

data_blocks(Sheet, Type, Blocks) :-
	findall(Block,
		(   sheet_object(Sheet, block, Block),
		    object_data_type(Block, Type)
		),
		Blocks0),
	resolve_intersections(Blocks0, Blocks).

resolve_intersections(Blocks0, Blocks) :-
	findall(B1-B2, block_intersection(Blocks0, B1, B2), Pairs),
	partition_graph(Pairs, Sets),
	maplist(block_union, Sets, Blocks).
resolve_intersections(Blocks0, Blocks) :-
	findall(i(B1,B2,Resolutions),
		( block_intersection(Blocks0, B1, B2),
		  intersection_resolutions(B1, B2, Resolutions)
		),
		Intersections),
	(   Intersections == []
	->  Blocks = Blocks0
	;   pp(Intersections)
	).

%%	block_intersection(+Blocks:list, -Intersection) is nondet.
%
%	True when Intersection describes  an   intersection  between two
%	datablocks  and  a  list  of  possible   ways  to  resolve  this
%	intersection.
%
%	@param Intersection is a term i(B1,B2,Resolutions)

block_intersection(Blocks, B1, B2) :-
	member(B1, Blocks),
	member(B2, Blocks),
	B1 \== B2,
	object_union(B1, Union1),
	object_union(B2, Union2),
	ds_intersection(Union1, Union2, _).

intersection_resolutions(B1, B2, Resolutions) :-
	findall(Resolve, resolve_intersection(B1, B2, Resolve),
		Resolutions).

%%	resolve_intersection(+B1, +B2, -Resolution) is nondet.
%
%	Resolve an intersection between B1 and B2.  Resolutions:
%
%	  * union(B1,B2,Problems)
%	  Create a union.  Problems are datasources that are included
%	  in the new union and nor part of B1, neither of B2 and are
%	  not empty.

resolve_intersection(B1, B2, union(B1,B2,Problems)) :-
	object_union(B1, Union1),
	object_union(B2, Union2),
	ds_union(Union1, Union2, Union),
	ds_subtract(Union1, Union, LocRest0),
	pairs_values(LocRest0, Rest0),
	maplist(ds_subtract(Union2), Rest0, NestedRests),
	append(NestedRests, LocRests),
	pairs_values(LocRests, Rests),
	exclude(ds_empty_cells, Rests, Problems).

ds_empty_cells(DS) :-
	ds_sheet(DS, Sheet),
	forall(ds_inside(DS, X, Y),
	       cell_class(Sheet,X,Y,empty)).


%%	block_union(+Blocks, -Block) is det.
%
%	True when Block is the union of Blocks.
%
%	@tbd	What should we do with the new parts that are included?

block_union([H|T], Union) :-
	block_union_list(T, H, Union).

block_union_list([], Union, Union).
block_union_list([H|T], Union0, Union) :-
	block_union(H, Union0, Union1),
	block_union_list(T, Union1, Union).

block_union(block(_, Type1, DS1),
	    block(_, Type2, DS2),
	    block(Id, Type, DS)) :-
	ds_union(DS1, DS2, DS),
	ds_id(DS, Id),
	type_union(Type1, Type2, Type).

type_union(Type1, Type2, Type) :-
	(   Type1 = Type2
	->  Type = Type1
	;   Type = hybrid
	).

%%	block_union_new_non_empty(+Blocks, -Union, -NonEmptyBlocks) is det.
%
%	Determine the union of Blocks and   unify NonEmptyDS with a list
%	of additional blocks that were added to   Union and are not part
%	of any block in Blocks.

block_union_new_non_empty(Blocks, UnionBlock, NonEmptyDS) :-
	block_union(Blocks, UnionBlock),
	object_union(UnionBlock, Union),
	maplist(object_union, Blocks, Parts),
	ds_sheet(Union, Sheet),
	findall(cell_range(Sheet,X,Y,X,Y),
		( ds_inside(Union, X, Y),
		  \+ ( member(Part, Parts),
		       ds_inside(Part, X, Y)
		     ),
		  \+ cell_class(Sheet, X, Y, empty)
		),
		NonEmptyCells),
	ds_join(NonEmptyCells, NonEmptyDS).

%%	partition_graph(+Edges, -VerticeSets) is det.
%
%	Partition a graph into a set of sets of connected vertices.

partition_graph(Edges, VerticeSets) :-
	vertices_edges_to_ugraph([], Edges, Graph),
	partition_graph2(Graph, VerticeSets).

partition_graph2([], []).
partition_graph2(Graph, [Set1|Sets]) :-
	Graph = [V0-_|_],
	reachable(V0, Graph, Set1),
	del_vertices(Graph, Set1, Graph2),
	partition_graph2(Graph2, Sets).


		 /*******************************
		 *	      BLOCKS		*
		 *******************************/

%%	assert_blocks(:Sheet, ?Type) is det.
%
%	Infer and assert identified blocks. Creates the following facts:
%
%	  * block(BlockID, Type, MainDS, HeaderDSList, UnionDS)
%	  * cell_property(Sheet, X, Y, block(BlockID))

assert_blocks(Sheet, Type) :-
	Sheet = M:_,
	blocks(Sheet, Type, Blocks),
	forall(member(T, Blocks),
	       assert_block(M:T)),
	(   Blocks == []
	->  true
	;   assert_blocks(Sheet, Type)
	).

%%	blocks(?Sheet, +Type, -Blocks) is det.
%
%	Make an initial guess  at  all  blocks.   Block  is  a  list  of
%	block(Data, Headers,Union).

blocks(Sheet, Type, Blocks) :-
	findall(SheetBlocks,
		( setof(Block,
			Type^block_in_sheet(Sheet, Type, Block),
			SheetBlocks0),
		  remove_inside(SheetBlocks0, SheetBlocks)
		),
		NestedBlocks),
	append(NestedBlocks, Blocks).

block_in_sheet(M:Sheet, Type, block(Id,Type,DS)) :-
	ds_sheet(DS, Sheet),
	cell_class(Type),
	unassigned_anchor(DS, Type),
	once(block(M:DS, Type)),
	ds_id(DS, Id, block).

%%	remove_inside(+Tables0, -Tables) is det.
%
%	Remove all tables that are entirely enclosed into other tables.

remove_inside(Tables0, Tables) :-
	remove_inside(Tables0, Tables0, Tables).

remove_inside([], _, []).
remove_inside([H|T0], All, T) :-
	arg(3, H, Union),
	member(T2, All),
	T2 \== H,
	arg(3, T2, U2),
	ds_intersection(Union, U2, Union), !,
	remove_inside(T0, All, T).
remove_inside([H|T0], All, [H|T]) :-
	remove_inside(T0, All, T).


%%	table(:DataDS, ?SupportDS) is nondet.
%
%	True when there is a table  with   DataDS  and a list of support
%	datasources.

table(QDataDS, TitleDS) :-
	QDataDS = _:DataDS,
	ds_size(DataDS, Cols, Rows),
	top_rows(QDataDS, -1, TitleDS, Left),
	left_columns(QDataDS, -1, Left, Right),
	right_columns(QDataDS, Cols, Right, Bottom),
	bottom_rows(QDataDS, Rows, Bottom, []).

%%	top_rows(:DS, +StartIndex, -Rows, ?Tail) is nondet.
%%	bottom_rows(:DS, +StartIndex, -Rows, ?Tail) is nondet.
%%	left_columns(:DS, +StartIndex, -Rows, ?Tail) is nondet.
%%	right_columns(:DS, +StartIndex, -Rows, ?Tail) is nondet.

top_rows(QDataDS, Index, [Row|Rows], Tail) :-
	QDataDS = M:DataDS,
	ds_unbounded_row_slice(DataDS, Index, Row),
	row(M:Row, string),
	Up is Index - 1,
	top_rows(QDataDS, Up, Rows, Tail).
top_rows(_, _, Tail, Tail).


bottom_rows(QDataDS, Index, [Row|Rows], Tail) :-
	QDataDS = M:DataDS,
	ds_unbounded_row_slice(DataDS, Index, Row),
	row(M:Row, string),
	Down is Index + 1,
	bottom_rows(QDataDS, Down, Rows, Tail).
bottom_rows(_, _, Tail, Tail).


left_columns(QDataDS, Index, [Col|Cols], Tail) :-
	QDataDS = M:DataDS,
	ds_unbounded_column_slice(DataDS, Index, Col),
	col(M:Col, string),
	Up is Index - 1,
	left_columns(QDataDS, Up, Cols, Tail).
left_columns(_, _, Tail, Tail).

right_columns(QDataDS, Index, [Col|Cols], Tail) :-
	QDataDS = M:DataDS,
	ds_unbounded_column_slice(DataDS, Index, Col),
	col(M:Col, string),
	Right is Index + 1,
	right_columns(QDataDS, Right, Cols, Tail).
right_columns(_, _, Tail, Tail).


		 /*******************************
		 *	  TABLE RELATIONS	*
		 *******************************/

%%	adjacent_objects(:Sheet, +Type, ?Obj1, ?Obj2, ?Rel)
%
%	True when Obj1 and Obj2 are adjacent in Sheet.  Rel is one of
%	=above=, =below= =left_of= or =right_of=

adjacent_objects(Sheet, Type, Obj1, Obj2, Rel) :-
	must_be(oneof([table,block]), Type),
	sheet_object(Sheet, Type, Obj1),
	sheet_object(Sheet, Type, Obj2),
	object_union(Obj1, Union1),
	object_union(Obj2, Union2),
	ds_adjacent(Union1, Rel, Union2).


%%	intersecting_objects(:Sheet, +Type, ?Obj1, ?Obj2, -Intersection)
%
%	True when Obj1 and Obj2 intersect  in Sheet. Intersection is the
%	intersecting part.

intersecting_objects(Sheet, Type, Obj1, Obj2, Intersection) :-
	must_be(oneof([table,block]), Type),
	sheet_object(Sheet, Type, Obj1),
	sheet_object(Sheet, Type, Obj2),
	Obj1 \== Obj2,
	object_union(Obj1, Union1),
	object_union(Obj2, Union2),
	ds_intersection(Union1, Union2, Intersection).


%%	color_sheets(?Sheet, ?What) is det.
%
%	Assign colours to objects in sheets. Colours are named 1,2,3,4.

color_sheets(Sheet, What) :-
	must_be(oneof([table,block]), What),
	Sheet = M:SheetName,
	forall(M:sheet(SheetName, _),
	       do_color_sheet(M:SheetName, What)).

do_color_sheet(Sheet, What) :-
	Sheet = _:SheetName,
	debug(color, 'Colouring sheet ~q', [SheetName]),
	color_adjacent(Sheet, What),
	color_intersecting_cells(Sheet, What).

color_adjacent(Sheet, What) :-
	Sheet = M:_,
	findall(color(T1,_)-color(T2,_),
		( (   adjacent_objects(Sheet, What, Tab1, Tab2, _)
		  ;   intersecting_objects(Sheet, What, Tab1, Tab2, _)
		  ),
		  object_id(Tab1, T1),
		  object_id(Tab2, T2)
		),
		Pairs),
	assign_vars(Pairs),
	maplist(color_constraint, Pairs),
	term_variables(Pairs, Colors),
	label(Colors), !,
	maplist(assign_color(M), Pairs).

%%	assign_vars(+Pairs)
%
%	Make sure each object id is associated with a unique variable.

assign_vars(List) :-
	empty_assoc(B0),
	assign_vars(List, B0).

assign_vars([], _).
assign_vars([color(O1,C1)-color(O2,C2)|T], B0) :-
	assign_var(O1, C1, B0, B1),
	assign_var(O2, C2, B1, B2),
	assign_vars(T, B2).

assign_var(Name, Var, B, B) :-
	get_assoc(Name, B, Var), !.
assign_var(Name, Var, B0, B) :-
	put_assoc(Name, B0, Var, B).

max_colors(10).

color_constraint(color(_,C1)-color(_,C2)) :-
	max_colors(Max),
	C1 in 1..Max,
	C2 in 1..Max,
	C1 #\= C2.

assign_color(M, color(T1,C1)-color(T2,C2)) :-
	assert_object_property(M:T1, color(C1)),
	assert_object_property(M:T2, color(C2)).

color_intersecting_cells(Sheet, What) :-
	forall(intersecting_objects(Sheet, What, Obj1, Obj2, Intersection),
	       ( object_id(Obj1, Id1),
		 object_id(Obj2, Id2),
		 forall(ds_inside(Intersection, X, Y),
			assert_cell_property(Sheet, X, Y, objects(Id1,Id2)))
	       )).


		 /*******************************
		 *	     LEFT-OVERS		*
		 *******************************/

%%	cells_outside_tables(+Sheet, +Tables, -Cells) is det.
%
%	True when Cells is a list of cell(Sheet,X,Y) that is outside any
%	table.

cells_outside_tables(Sheet, Tables, Cells) :-
	findall(cell(Sheet,X,Y),
		( sheet_bb(Sheet, SheetDS),
		  ds_inside(SheetDS, X, Y),
		  cell_value(Sheet, X, Y, _),
		  \+ ( member(table(_,_,DS), Tables),
		       ds_inside(DS,X,Y)
		     )
		),
		Cells).
