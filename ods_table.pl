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
	       load_column(Col, Style, State, Module)).

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
	retractall(M:cell(_,_,_,_,_,_)),
	retractall(M:style(_,_)).
