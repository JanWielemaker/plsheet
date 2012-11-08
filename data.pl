:- module(ods_data,
	  [ assert_table/1,		% +Table

	    sheet_table/2,		% ?Sheet, ?Table
	    table_union/2,		% ?Table, ?Union

	    assert_cell_property/4,	% :Sheet, +X, +Y, ?Property
	    cell_property/4		% :Sheet, ?X, ?Y, ?Property
	  ]).
:- use_module(datasource).

:- meta_predicate
	assert_table(:),
	sheet_table(:, ?),
	assert_cell_property(:, +, +, +),
	cell_property(:,?,?,?).


/** <module> Data store module

Defined relations:

  * table(TabId, Type, DataDS, HeaderDSList, UnionDS)
  * cell_property(Sheet, X, Y, Property)
*/

		 /*******************************
		 *	       TABLES		*
		 *******************************/

%%	assert_table(:Table) is det.
%
%	@param Table is table(TabId, _Type, _MainDS, _HdrDS, Union)

assert_table(M:T) :-
	assertz(M:T),
	T = table(TabId, _Type, _MainDS, _HdrDS, Union),
	ds_sheet(Union, Sheet),
	forall(ds_inside(Union, X, Y),
	       assert_cell_property(M:Sheet, X, Y, table(TabId))).

%%	sheet_table(:Sheet, ?Table)
%
%	True when Sheet contains Table.  Table is a struct
%
%	  ==
%	  table(TabId, Type, DataDS, HeaderDSList, UnionDS)
%	  ==

sheet_table(M:Sheet, table(TabId, Type, DataDS, HdrDS, Union)) :-
	ds_sheet(DataDS, Sheet),
	M:table(TabId, Type, DataDS, HdrDS, Union).


%%	table_union(+Table, -Union) is det.
%
%	True uf Union is the UnionDS of Table.

table_union(table(_TabId, _Type, _DataDS, _HdrDS, Union), Union).


		 /*******************************
		 *	       CELLS		*
		 *******************************/

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


