:- module(table,
	  [ tables/3,			% ?Sheet, +Type, -Tables
	    table/2,			% +Data, -Support
	    cells_outside_tables/3	% +Sheet, +Table, -Cells
	  ]).
:- use_module(recognise).
:- use_module(datasource).
:- use_module(library(lists)).

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

table_in_sheet(Sheet, Type, table(DS,Headers,Union)) :-
	ds_sheet(DS, Sheet),
	cell_class(Type),
	anchor(DS, Type),
	once((block(DS, Type),
	      table(DS, Headers))),
	ds_union([DS|Headers], Union).

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


%%	cells_outside_tables(+Sheet, +Table, -Cells) is det.
%
%	True when Cells is a list of cell(Sheet,X,Y) that is outside any
%	table.

cells_outside_tables(Sheet, Tables, Cells) :-
	findall(cell(Sheet,X,Y),
		( sheet_bb(Sheet, SX,SY, EX,EY),
		  between(SX, EX, X),
		  between(SY, EY, Y),
		  cell_value(Sheet, X, Y, _),
		  \+ ( member(table(_,_,DS), Tables),
		       ds_inside(DS,X,Y)
		     )
		),
		Cells).


%%	table(+DataDS, ?SupportDS) is nondet.
%
%	True when there is a table  with   DataDS  and a list of support
%	datasources.

table(DataDS, TitleDS) :-
	ds_size(DataDS, Cols, Rows),
	top_rows(DataDS, -1, TitleDS, Left),
	left_columns(DataDS, -1, Left, Right),
	right_columns(DataDS, Cols, Right, Bottom),
	bottom_rows(DataDS, Rows, Bottom, []).

%%	top_rows(+DS, +StartIndex, -Rows, ?Tail) is nondet.
%%	bottom_rows(+DS, +StartIndex, -Rows, ?Tail) is nondet.
%%	left_columns(+DS, +StartIndex, -Rows, ?Tail) is nondet.
%%	right_columns(+DS, +StartIndex, -Rows, ?Tail) is nondet.

top_rows(DataDS, Index, [Row|Rows], Tail) :-
	ds_unbounded_row_slice(DataDS, Index, Row),
	row(Row, string),
	Up is Index - 1,
	top_rows(DataDS, Up, Rows, Tail).
top_rows(_, _, Tail, Tail).


bottom_rows(DataDS, Index, [Row|Rows], Tail) :-
	ds_unbounded_row_slice(DataDS, Index, Row),
	row(Row, string),
	Down is Index + 1,
	bottom_rows(DataDS, Down, Rows, Tail).
bottom_rows(_, _, Tail, Tail).


left_columns(DataDS, Index, [Col|Cols], Tail) :-
	ds_unbounded_column_slice(DataDS, Index, Col),
	col(Col, string),
	Up is Index - 1,
	left_columns(DataDS, Up, Cols, Tail).
left_columns(_, _, Tail, Tail).

right_columns(DataDS, Index, [Col|Cols], Tail) :-
	ds_unbounded_column_slice(DataDS, Index, Col),
	col(Col, string),
	Right is Index + 1,
	right_columns(DataDS, Right, Cols, Tail).
right_columns(_, _, Tail, Tail).

