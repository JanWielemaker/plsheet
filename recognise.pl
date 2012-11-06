:- module(recognise,
	  [ block/2,			% DataSource, Type
	    row/2,			% DataSource, Type
	    col/2,			% DataSource, Type

	    row/6,			% Sheet, SX,SY, EX,SY, Type
	    col/6,			% Sheet, SX,SY, EX,SY, Type
	    block/6,			% Sheet, SX,SY, EX,SY, Type

	    sheet_bb/5			% :Sheet, SX,SY, EX,SY
	  ]).
:- use_module(ods_table).
:- use_module(library(apply)).
:- use_module(library(lists)).

:- meta_predicate
	sheet_bb(:,?,?,?,?).


/** <module> Inference over spreadsheets

@tbd	Use constraints for types?  E.g., allow for float or empty
*/

%%	block(?DataSource, ?Type) is nondet.

block(cell_range(Sheet, SX,SY, EX,EY), Type) :-
	block(Sheet, SX,SY, EX,EY, Type).

row(cell_range(Sheet, SX,SY, EX,EY), Type) :-
	row(Sheet, SX,SY, EX,EY, Type).

col(cell_range(Sheet, SX,SY, EX,EY), Type) :-
	col(Sheet, SX,SY, EX,EY, Type).


%%	block(?Sheet, ?SX,?SY, ?EX, ?EY, ?Type) is nondet.

block(Sheet, SX,SY, EX,EY, Type) :-
	row(Sheet, SX,SY, EX,SY, Type),
	Y2 is SY+1,
	block2(Sheet, SX,Y2,EX,EY, Type).

block2(Sheet, SX,SY,EX,EY, Type) :-
	row(Sheet, SX,SY, EX,SY, Type),
	Y2 is SY+1,
	(   block2(Sheet, SX,Y2,EX,EY, Type)
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
	(   cell_type(Sheet, X,Y, Type)
	->  true
	;   Type = empty
	->  sheet_bb(Sheet, MinX,MinY,MaxX,MaxY),
	    between(MinX, MaxX, X),
	    between(MinY, MaxY, Y)
	).
cell_class(Sheet, X,Y, Type) :-
	Type == empty, !,
	sheet_bb(Sheet, MinX,MinY,MaxX,MaxY),
	between(MinX, MaxX, X),
	between(MinY, MaxY, Y),
	\+ cell_type(Sheet, X,Y, Type).
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
