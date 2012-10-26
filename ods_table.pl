:- module(ods_table,
	  [ ods_DOM/3,			% +File, -DOM, +Options
	    ods_load/1,			% :DOM
	    ods_clean/0
	  ]).
:- use_module(library(xpath)).

:- meta_predicate
	ods_load(:).

%%	ods_DOM(+File -DOM, +Options) is det.
%
%	DOM is the XML domtree of  the   content  file  of the given ODS
%	document.

ods_DOM(File, DOM, Options) :-
	setup_call_cleanup(
	    archive_open(File, Archive, []),
	    archive_dom(Archive, DOM, Options),
	    archive_close(Archive)).

archive_dom(Archive, DOM, Options) :-
	option(member(Member), Options, 'content.xml'),
	archive_next_header(Archive, Member),
	setup_call_cleanup(
	    archive_open_entry(Archive, Stream),
	    load_structure(Stream, DOM, Options),
	    close(Stream)).

%%	ods_load(:DOM)
%
%	Convert tables in DOM into a  set   of  Prolog predicates in the
%	calling module.  The generated predicates are:
%
%	    - table(Name, Style)
%	    - col(Table, X, Style)
%	    - row(Table, Y, Style)
%	    - cell(Table, X, Y, Value, Type, Formula, Style)
%	    - style(Style, Properties)

ods_load(Module:DOM) :-
	DOM = [element(_,_,_)], !,
	load_styles(DOM, Module),
	load_tables(DOM, Module).
ods_load(Module:File) :-
	ods_DOM(File, DOM, []),
	ods_load(Module:DOM).


load_styles(_, _).

load_tables(DOM, Module) :-
	forall(xpath(DOM, //'table:table'(@'table:name'=Name,
					@'table:style-name'=Style), Table),
	       load_table(Table, Name, Style, Module)).

load_table(DOM, Name, TStyle, Module) :-
	assertz(Module:table(Name, TStyle)),
	State = state(1,1,Name,_),
	forall(xpath(DOM, 'table:table-column'(@'table:style-name'=Style), Col),
	       load_column(Col, Style, State, Module)),
	forall(xpath(DOM, 'table:table-row'(@'table:style-name'=Style), Col),
	       load_row(Col, Style, State, Module)).

load_column(element(_, CollAttrs, []), Style, State, Module) :-
	arg(1, State, X0),
	arg(3, State, Table),
	(   memberchk('table:number-columns-repeated'=RepA, CollAttrs),
	    atom_number(RepA, Rep)
	->  true
	;   Rep = 1
	),
	End is X0+Rep-1,
	forall(between(X0, End, X),
	       assertz(Module:col(Table, X, Style))),
	NextX is End+1,
	nb_setarg(1, State, NextX).

load_row(DOM, Style, State, Module) :-
	DOM = element(_, _RowAttrs, _),
	nb_setarg(1, State, 1),
	arg(2, State, Y),
	arg(3, State, Table),
	assertz(Module:row(Table, Y, Style)),
	forall(xpath(DOM, 'table:table-cell'(@'table:style-name'=CStyle), Cell),
	       load_cell(Cell, CStyle, State, Module)),
	NextY is Y + 1,
	nb_setarg(2, State, NextY).

load_cell(DOM, Style, State, Module) :-
	DOM = element(_, CellAttrs, Content),
	arg(1, State, X0),
	arg(2, State, Y),
	arg(3, State, Table),
	(   memberchk('table:number-columns-repeated'=RepA, CellAttrs),
	    atom_number(RepA, Rep)
	->  true
	;   Rep = 1
	),
	End is X0+Rep-1,
	(   Content == []
	->  true
	;   cell_type(DOM, Type),
	    cell_value(DOM, Type, Value),
	    cell_formula(DOM, Formula),
	    forall(between(X0, End, X),
		   assertz(Module:cell(Table,X,Y,Value,Type,Formula,Style)))
	),
	NextX is End+1,
	nb_setarg(1, State, NextX).

cell_type(DOM, Type) :-
	xpath(DOM, /'table:table-cell'(@'office:value-type'), OfficeType),
	OfficeType = Type.

cell_value(DOM, Type, Value) :-
	xpath(DOM, /'table:table-cell'(@'office:value'), OfficeValue), !,
	convert_value(Type, OfficeValue, Value).
cell_value(DOM, string, Value) :-
	xpath(DOM, /'table:table-cell'(normalize_space), Value).

convert_value(float, Text, Value) :- !,
	(   atom_number(Text, Value0)
	->  Value is float(Value0)
	;   type_error(float, Text)
	).
convert_value(percentage, Text, Value) :- !,
	(   atom_number(Text, Value0)
	->  Value is float(Value0)
	;   type_error(percentage, Text)
	).
convert_value(Type, Value, Value) :-
	print_message(warning, ods(unknown_type(Type))).

cell_formula(_, -).


%%	ods_clean
%
%	Remove saved facts from the database

:- module_transparent
	ods_clean/0.

ods_clean :-
	context_module(M),
	retractall(M:table(_,_)),
	retractall(M:col(_,_,_)),
	retractall(M:row(_,_,_)),
	retractall(M:cell(_,_,_,_,_,_,_)),
	retractall(M:style(_,_)).
