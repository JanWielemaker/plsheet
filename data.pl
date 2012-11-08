:- module(ods_data,
	  [ assert_table/1,		% +Table
	    assert_cell_property/4,	% :Sheet, +X, +Y, ?Property
	    cell_property/4		% :Sheet, ?X, ?Y, ?Property
	  ]).
:- use_module(datasource).

:- meta_predicate
	assert_table(:),
	assert_cell_property(:, +, +, +),
	cell_property(:,?,?,?).


/** <module> Data store module

Defined relations:

  * table(TabId, Type, DataDS, HeaderDSList, UnionDS)
  * cell_property(Sheet, X, Y, Property)
*/

%%	assert_table(:Table) is det.
%
%	@param Table is table(TabId, _Type, _MainDS, _HdrDS, Union)

assert_table(M:T) :-
	assertz(M:T),
	T = table(TabId, _Type, _MainDS, _HdrDS, Union),
	ds_sheet(Union, Sheet),
	forall(ds_inside(Union, X, Y),
	       assert_cell_property(M:Sheet, X, Y, table(TabId))).

%%	assert_cell_property(:Sheet, +X, +Y, +Property) is det.
%
%	Add a property to a  cell.  Does   nothing  if  the  property is
%	already defined.


assert_cell_property(M:Sheet, X, Y, Property) :-
	cell_id(X,Y,CellId),
	assertz(M:cell_property(Sheet,CellId,Property)).


%%	cell_property(:Sheet, ?X, ?Y, ?Property)
%
%	Query (inferred) properties of the cell Sheet.XY.

cell_property(M:Sheet, X, Y, Property) :-
	(   nonvar(X), nonvar(Y)
	->  cell_id(X,Y,Id),
	    M:cell_property(Sheet,Id,Property)
	;   M:cell_property(Sheet,Id,Property),
	    cell_id(X,Y,Id)
	).


