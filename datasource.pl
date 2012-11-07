:- module(datasource,
	  [ ds_sheet/2,			% +DS, -Sheet
	    ds_size/3,			% +DS, -Columns, -Rows

	    ds_inside/3,		% +DS, ?X, ?Y

	    ds_intersection/3,		% +DS1, +DS2, -DS
	    ds_union/3,			% +DS1, +DS2, -DS
	    ds_union/2,			% +DSList, -DS
	    ds_intersections/2,		% +DSList, -Pairs

	    ds_row_slice/3,		% +DS1, ?Offset, ?Slice
	    ds_unbounded_row_slice/3,	% +DS1, +Offset, ?Slice
	    ds_column_slice/3,		% +DS1, ?Offset, ?Slice
	    ds_unbounded_column_slice/3	% +DS1, +Offset, ?Slice
	  ]).

		 /*******************************
		 *	 SIMPLE PROPERTIES	*
		 *******************************/

%%	ds_sheet(+DS, -Sheet) is det.
%
%	True when DS is on Sheet.

ds_sheet(cell_range(Sheet, _,_, _,_), Sheet).

%%	ds_size(+DS, -Columns, -Rows) is det.
%
%	True when Columns and Rows represent the size of a datasource

ds_size(cell_range(_Sheet, SX,SY, EX,EY), Columns, Rows) :-
	Columns is EX-SX+1,
	Rows is EY-SY+1.


		 /*******************************
		 *	    COORDINATES		*
		 *******************************/

%%	ds_inside(+DS, ?X, ?Y) is nondet.
%
%	True when X,Y is inside the datasource

ds_inside(cell_range(_Sheet, SX,SY, EX,EY), X, Y) :-
	between(SY, EY, Y),
	between(SX, EX, X).


		 /*******************************
		 *	     SET LOGIC		*
		 *******************************/

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


%%	ds_intersections(+ListOfDS, -Pairs) is semidet.
%
%	True when Pairs is a non-empty list of pairs of datasources with
%	a non-empty intersection.
%
%	@tbd	Can be more efficient

ds_intersections(ListOfDS, Pairs) :-
	findall(A-B,
		( member(A,ListOfDS),
		  member(B,ListOfDS),
		  A@>B,
		  ds_intersection(A,B,_)
		),
		Pairs),
	Pairs \== [].



		 /*******************************
		 *	      SLICING		*
		 *******************************/

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

