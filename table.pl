:- module(table,
	  [ assert_tables/2,		% ?Sheet, ?Type
	    tables/3,			% ?Sheet, +Type, -Tables
	    table/2,			% +Data, -Support

	    adjacent_tables/4,		% :Sheet, ?Tab1, ?Rel, ?Tab2

	    cells_outside_tables/3	% +Sheet, +Table, -Cells
	  ]).
:- use_module(recognise).
:- use_module(datasource).
:- use_module(ods_table).
:- use_module(data).
:- use_module(library(lists)).

:- meta_predicate
	tables(:, ?, -),
	assert_tables(:, ?),
	adjacent_tables(:, ?, ?, ?).

/** <module> Detect tables
*/

%%	assert_tables(:Sheet, ?Type) is det.
%
%	Infer and assert identified tables. Creates the following facts:
%
%	  * table(TableID, Type, MainDS, HeaderDSList, UnionDS)
%	  * cell_property(Sheet CellId, table(TableID))

assert_tables(Sheet, Type) :-
	Sheet = M:_,
	tables(Sheet, Type, Tables),
	forall(member(T, Tables),
	       assert_table(M:T)).

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
	anchor(DS, Type),
	once((block(M:DS, Type),
	      table(M:DS, Headers))),
	ds_union([DS|Headers], Union),
	ds_id(DS, Id).

%%	remove_inside(+Tables0, -Tables) is det.
%
%	Remove all tables that are entirely enclosed into other tables.

remove_inside(Tables0, Tables) :-
	remove_inside(Tables0, Tables0, Tables).

remove_inside([], _, []).
remove_inside([H|T0], All, T) :-
	H = table(_,_,Union),
	member(T2, All),
	T2 \== H,
	T2 = table(_,_,U2),
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

%%	adjacent_tables(:Sheet, ?Tab1, ?Rel, ?Tab2)
%
%	True when Tab1 and Tab2 are adjacent in Sheet.  Rel is one of
%	=above=, =below= =left_of= or =right_of=

adjacent_tables(Sheet, Tab1, Rel, Tab2) :-
	sheet_table(Sheet, Tab1),
	sheet_table(Sheet, Tab2),
	table_union(Tab1, Union1),
	table_union(Tab2, Union2),
	ds_adjacent(Union1, Rel, Union2).


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
