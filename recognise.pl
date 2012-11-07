:- module(recognise,
	  [ anchor/2,			% DataSource, Type

	    block/2,			% DataSource, Type
	    row/2,			% DataSource, Type
	    col/2,			% DataSource, Type

	    row/6,			% Sheet, SX,SY, EX,SY, Type
	    col/6,			% Sheet, SX,SY, EX,SY, Type
	    block/6,			% Sheet, SX,SY, EX,SY, Type

	    cell_class/4,		% :Sheet, ?SX, ?SY, ?Class

	    sheet_bb/5,			% :Sheet, SX,SY, EX,SY

	    ds_sheet/2,			% +DS, -Sheet
	    ds_size/3,			% +DS, -Columns, -Rows

	    ds_intersection/3,		% +DS1, +DS2, -DS
	    ds_union/3,			% +DS1, +DS2, -DS
	    ds_union/2,			% +DSList, -DS

	    ds_row_slice/3,		% +DS1, ?Offset, ?Slice
	    ds_unbounded_row_slice/3,	% +DS1, +Offset, ?Slice
	    ds_column_slice/3,		% +DS1, ?Offset, ?Slice
	    ds_unbounded_column_slice/3	% +DS1, +Offset, ?Slice
	  ]).
:- use_module(ods_table).
:- use_module(library(apply)).
:- use_module(library(lists)).

:- meta_predicate
	anchor(:, ?),

	block(:,?),
	row(:,?),
	col(:,?),

	block(:,?,?,?,?,?),
	row(:,?,?,?,?,?),
	col(:,?,?,?,?,?),

	cell_class(:, ?, ?, ?),

	sheet_bb(:,?,?,?,?).


/** <module> Inference over spreadsheets

@tbd	Use constraints for types?  E.g., allow for float or empty
*/

%%	anchor(:DataSource, ?Type) is nondet.
%
%	True when the top-level of DataSource is an anchor of Type. This
%	implies it is a cell the given Type and the cells left and above
%	it are of different types.
%
%	The anchor/2 predicate  is  used   to  generate  candidates  for
%	creating larger units of cells.  For   example,  to generate all
%	blocks of floats, use this:
%
%	  ==
%	  ?- anchor(D, float), once(block(D, float)).
%	  ==

anchor(M:cell_range(Sheet, SX,SY, _EX,_EY), Type) :-
	cell_class(M:Sheet, SX,SY, Type),
	(   SX =:= 0
	->  true
	;   LX is SX-1,
	    cell_class(M:Sheet, LX,SY, TLeft), TLeft \== Type
	),
	(   SY =:= 0
	->  true
	;   AY is SY-1,
	    cell_class(M:Sheet, SX,AY, TAbove), TAbove \== Type
	).

%%	block(?DataSource, ?Type) is nondet.

block(M:cell_range(Sheet, SX,SY, EX,EY), Type) :-
	block(M:Sheet, SX,SY, EX,EY, Type).

row(M:cell_range(Sheet, SX,SY, EX,EY), Type) :-
	row(M:Sheet, SX,SY, EX,EY, Type).

col(M:cell_range(Sheet, SX,SY, EX,EY), Type) :-
	col(M:Sheet, SX,SY, EX,EY, Type).


%%	block(?Sheet, ?SX,?SY, ?EX, ?EY, ?Type) is nondet.

block(Sheet, SX,SY, EX,EY, Type) :-
	row(Sheet, SX,SY, EX,SY, Type),
	block2(Sheet, SX,SY,EX,EY, Type).

block2(Sheet, SX,SY,EX,EY, Type) :-
	(   Y2 is SY+1,
	    row(Sheet, SX,Y2, EX,Y2, Type),
	    block2(Sheet, SX,Y2,EX,EY, Type)
	;   EY=SY
	).

row(Sheet, SX,SY, EX,SY, Type) :-
	cell_class(Sheet, SX,SY, Type),
	X2 is SX+1,
	row2(Sheet, X2,SY, EX,SY, Type).

row2(Sheet, SX,SY, EX,SY, Type) :-
	cell_class(Sheet, SX,SY, Type),
	X2 is SX+1,
	(   row2(Sheet, X2,SY, EX,SY, Type)
	;   EX=SX
	).


col(Sheet, SX,SY, SX,EY, Type) :-
	cell_class(Sheet, SX,SY, Type),
	Y2 is SY+1,
	col2(Sheet, SX,Y2, SX,EY, Type).

col2(Sheet, SX,SY, SX,EY, Type) :-
	cell_class(Sheet, SX,SY, Type),
	Y2 is SY+1,
	(   col2(Sheet, SX,Y2, SX,EY, Type)
	;   EY=SY
	).


		 /*******************************
		 *	   BLANK PARTS		*
		 *******************************/

%%	cell_class(?Sheet, ?X,?Y, ?Class) is	nondet.
%
%	Classification of cells.  Defined classes are:
%
%	  * string
%	  * float
%	  * percentage
%	  * empty
%	  * style(Style)

cell_class(Sheet, X,Y, Type) :-
	ground(cell(Sheet,X,Y)), !,
	(   cell_type(Sheet, X,Y, Type0)
	->  Type = Type0
	;   Type = empty
	->  sheet_bb(Sheet, MinX,MinY,MaxX,MaxY),
	    between(MinX, MaxX, X),
	    between(MinY, MaxY, Y),
	    \+ cell_type(Sheet, X,Y, _)
	).
cell_class(Sheet, X,Y, Type) :-
	Type == empty, !,
	sheet_bb(Sheet, MinX,MinY,MaxX,MaxY),
	between(MinX, MaxX, X),
	between(MinY, MaxY, Y),
	\+ cell_type(Sheet, X,Y, _).
cell_class(Sheet, X,Y, Type) :-
	cell_type(Sheet, X,Y, Type).


%%	sheet_bb(:Sheet, ?SX,?SY, ?EX,?EY) is nondet.
%
%	True if Sheet is covered by the given bounding box.  Note that
%	SX and SY may be 0.  Fails of the sheet is empty.

:- dynamic
	sheet_bb_cache/6,
	sheet_bb_cached/2.

sheet_bb(M:Sheet, SX,SY,EX,EY) :-
	M:sheet(Sheet, _),
	(   sheet_bb_cached(M, Sheet)
	->  sheet_bb_cache(M, Sheet, SX,SY,EX,EY)
	;   sheet_bb(M, Sheet, SX0,SY0,EX0,EY0)
	->  assertz(sheet_bb_cached(M, Sheet)),
	    assertz(sheet_bb_cache(M, Sheet, SX0,SY0,EX0,EY0)),
	    SX=SX0, SY=SY0, EX=EX0,EY=EY0
	;   assertz(sheet_bb_cached(M, Sheet)), % empty sheet
	    fail
	).

sheet_bb(M, Sheet, SX,SY,EX,EY) :-
	M:sheet(Sheet, _),
	findall(X-Y, cell_exists(M:Sheet, X,Y), Pairs),
	maplist(arg(1), Pairs, AtCol),
	min_list(AtCol, MinX),
	max_list(AtCol, MaxX),
	maplist(arg(2), Pairs, AtRow),
	min_list(AtRow, MinY),
	max_list(AtRow, MaxY),
	SX is MinX - 1,
	SY is MinY - 1,
	EX is MaxX + 1,
	EY is MaxY + 1.

cell_exists(M:Sheet,X,Y) :-
	cell(M:Sheet, X,Y, _,_,_,_,_).


		 /*******************************
		 *	 DATASOURCE LOGIC	*
		 *******************************/

%%	ds_sheet(+DS, -Sheet) is det.
%
%	True when DS is on Sheet.

ds_sheet(cell_range(Sheet, _,_, _,_), Sheet).


%%	ds_intersection(+DS1, +DS2, -DS) is semidet.
%
%	True when the intersection of DS1 and DS2 is DS.  Fails if the
%	two do not intersect.

ds_intersection(cell_range(Sheet, SX1,SY1, EX1,EY1),
		cell_range(Sheet, SX2,SY2, EX2,EY2),
		cell_range(Sheet, SX,SY, EX,EY)) :-
	range_intersect(SX1,EX1, SX2,EX2, SX,EX),
	range_intersect(SY1,EY1, SY2,EY2, SY,EY).

range_intersect(S1,E1, S2,E2, S,E) :-
	S is max(S1,S2),
	E is min(E1,E2),
	S =< E.


%%	ds_union(+DS1, +DS2, -DS) is det.
%
%	True when the union of DS1 and DS2 is DS.

ds_union(cell_range(Sheet, SX1,SY1, EX1,EY1),
	 cell_range(Sheet, SX2,SY2, EX2,EY2),
	 cell_range(Sheet, SX,SY, EX,EY)) :-
	range_union(SX1,EX1, SX2,EX2, SX,EX),
	range_union(SY1,EY1, SY2,EY2, SY,EY).

range_union(S1,E1, S2,E2, S,E) :-
	S is min(S1,S2),
	E is max(E1,E2).


%%	ds_union(+DSList, -DS) is det.
%
%	True when DS is the union of all datasources

ds_union([], cell_range(_, 0,0,0,0)).
ds_union([H|T], Union) :-
	ds_union_list(T, H, Union).

ds_union_list([], DS, DS).
ds_union_list([H|T], DS0, DS) :-
	ds_union(H, DS0, DS1),
	ds_union_list(T, DS1, DS).


%%	ds_size(+DS, -Columns, -Rows) is det.
%
%	True when Columns and Rows represent the size of a datasource

ds_size(cell_range(_Sheet, SX,SY, EX,EY), Columns, Rows) :-
	Columns is EX-SX+1,
	Rows is EY-SY+1.


%%	ds_row_slice(+DS, ?Offset, ?Slice) is det.
%
%	True when Slice is a row from   DS at offset Offset. Offsets are
%	0-based.

ds_row_slice(cell_range(Sheet, SX,SY, EX,EY), Offset,
	     cell_range(Sheet, SX,RY, EX,RY)) :-
	H is EY-SY,
	between(0,H,Offset),
	RY is SY+Offset.


%%	ds_unbounded_row_slice(+DS, +Offset, -Slice) is det.
%
%	True when Slice is a row from   DS at offset Offset. Offsets are
%	0-based. It is allowed for Slice to  be outside the range of the
%	datasouce.

ds_unbounded_row_slice(cell_range(Sheet, SX,SY, EX,_), Offset,
	     cell_range(Sheet, SX,RY, EX,RY)) :-
	RY is SY+Offset.

%%	ds_column_slice(+DS, ?Offset, ?Slice) is det.
%
%	True when Slice is a column from   DS  at offset Offset. Offsets
%	are 0-based.

ds_column_slice(cell_range(Sheet, SX,SY, EX,EY), Offset,
		cell_range(Sheet, CX,SY, CX,EY)) :-
	W is EX-SX,
	between(0,W,Offset),
	CX is SX+Offset.

%%	ds_unbounded_column_slice(+DS, +Offset, -Slice) is det.
%
%	True when Slice is a column from   DS at offset Offset. Offsets are
%	0-based. It is allowed for Slice to  be outside the range of the
%	datasouce.

ds_unbounded_column_slice(cell_range(Sheet, SX,SY,  _,EY), Offset,
			  cell_range(Sheet, CX,SY, CX,EY)) :-
	CX is SX+Offset.
