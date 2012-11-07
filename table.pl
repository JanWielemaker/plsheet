:- module(table,
	  [ table/2			% +Data, -Support
	  ]).
:- use_module(recognise).

%%	table(+DataDS, ?SupportDS) is nodet.
%
%	True when there is a table  with   DataDS  and a list of support
%	datasources.

table(DataDS, TitleDS) :-
	ds_size(DataDS, Cols, Rows),
	top_rows(DataDS, -1, TitleDS, Left),
	left_columns(DataDS, -1, Left, Right),
	right_columns(DataDS, Cols+1, Right, Bottom),
	bottom_rows(DataDS, Rows+1, Bottom, []).

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
