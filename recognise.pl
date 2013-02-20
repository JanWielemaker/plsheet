:- module(recognise,
	  [ anchor/2,			% :DataSource, Type
	    unassigned_anchor/2,	% :DataSource, ?Type

	    block/2,			% :DataSource, Type
	    row/2,			% :DataSource, Type
	    col/2,			% :DataSource, Type

	    row/6,			% :Sheet, SX,SY, EX,SY, Type
	    col/6,			% :Sheet, SX,SY, EX,SY, Type
	    block/6,			% :Sheet, SX,SY, EX,SY, Type

	    cell_class/1,		% ?Class
	    cell_class/4,		% :Sheet, ?SX, ?SY, ?Class

	    sheet_bb/2,			% :Sheet, -DataSource
	    ds_join/2			% +DataSources, -Joined
	  ]).
:- use_module(ods_table).
:- use_module(datasource).
:- use_module(data).
:- use_module(library(apply)).
:- use_module(library(lists)).

:- meta_predicate
	anchor(:, ?),
	unassigned_anchor(:, ?),

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

%%	unassigned_anchor(:DataSource, ?Type) is nondet.

unassigned_anchor(M:cell_range(Sheet, SX,SY, _EX,_EY), Type) :-
	MSheet = M:Sheet,
	cell_class(MSheet, SX,SY, Type),
	\+ assigned(MSheet, SX,SY),
	(   SX =:= 0
	->  true
	;   LX is SX-1,
	    (	cell_class(MSheet, LX,SY, TLeft), TLeft \== Type
	    ->	true
	    ;	assigned(MSheet, LX,SY)
	    )
	),
	(   SY =:= 0
	->  true
	;   AY is SY-1,
	    (	cell_class(MSheet, SX,AY, TAbove), TAbove \== Type
	    ->	true
	    ;	assigned(MSheet, SX,AY)
	    )
	).

assigned(Sheet, X, Y) :-
	cell_property(Sheet, X, Y, P),
	assigned(P).

assigned(table(_)).
assigned(block(_)).


%%	block(:DataSource, ?Type) is nondet.

block(M:cell_range(Sheet, SX,SY, EX,EY), Type) :-
	block(M:Sheet, SX,SY, EX,EY, Type).

row(M:cell_range(Sheet, SX,SY, EX,EY), Type) :-
	row(M:Sheet, SX,SY, EX,EY, Type).

col(M:cell_range(Sheet, SX,SY, EX,EY), Type) :-
	col(M:Sheet, SX,SY, EX,EY, Type).


%%	block(?Sheet, ?SX,?SY, ?EX, ?EY, ?Type) is nondet.
%
%	A block is the largest rectangular  area   of  cells of the same
%	Type that starts at SX,SY.  A   block  consists minimally of two
%	cells, stacked either horizontally or vertically.

block(Sheet, SX,SY, EX,EY, Type) :-
	row(Sheet, SX,SY, EX,SY, Type), !,		% dubious cut
	block2(Sheet, SX,SY,EX,EY, Type).
block(Sheet, SX,SY, EX,EY, Type) :-
	col(Sheet, SX,SY, EX,EY, Type).

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

cell_class(float).
cell_class(percentage).
cell_class(string).


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
	(   cell_type(Sheet, X,Y, Type0),
	    Type0 \== no_type,
	    \+ cell_value(Sheet, X, Y, '')
	->  Type = Type0
	;   Type = empty
	->  sheet_bb(Sheet, SheetDS),
	    ds_grow(SheetDS, 1, ExtendedDS),
	    ds_inside(ExtendedDS, X, Y),
	    empty_cell(Sheet, X,Y)
	).
cell_class(Sheet, X,Y, Type) :-
	Type == empty, !,
	sheet_bb(Sheet, SheetDS),
	ds_grow(SheetDS, 1, ExtendedDS),
	ds_inside(ExtendedDS, X, Y),
	empty_cell(Sheet, X,Y).
cell_class(Sheet, X,Y, Type) :-
	cell_type(Sheet, X,Y, Type).

empty_cell(Sheet, X, Y) :-
	cell_type(Sheet, X,Y, Type), !,
	(   Type == string
	->  cell_value(Sheet, X, Y, '')
	;   Type == no_type
	).
empty_cell(_,_,_).



%%	sheet_bb(:Sheet, ?DS) is nondet.
%
%	True if DS is a datasource that   describes  all cells in Sheet.
%	Fails of the sheet is empty.

:- dynamic
	sheet_bb_cache/6,
	sheet_bb_cached/2.

sheet_bb(M:Sheet, cell_range(Sheet,SX,SY,EX,EY)) :-
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
	min_list(AtCol, SX),
	max_list(AtCol, EX),
	maplist(arg(2), Pairs, AtRow),
	min_list(AtRow, SY),
	max_list(AtRow, EY).

cell_exists(M:Sheet,X,Y) :-
	cell(M:Sheet, X,Y, _,_,_,_,_),
	\+ empty_cell(M:Sheet, X, Y).

%%	ds_join(+DSList, -GroupedDSList) is det.
%
%	Create larger datasources by grouping adjacent ones.

ds_join(List, List) :-
	tbd.
