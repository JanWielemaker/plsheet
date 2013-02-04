:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

gv_attrs :-
	forall(gv_attrs(gv_attr(Names, On, Type)),
	       (   is_list(Names)
	       ->  forall(member(Name, Names),
			  portray_clause(gv_attr(Name, On, Type)))
	       ;   portray_clause(gv_attr(Names, On, Type))
	       )).


gv_attrs(gv_attr(Name, On, Type)) :-
	dtd(html, DTD),
	setup_call_cleanup(
	    http_open('http://www.graphviz.org/content/attrs', In, []),
	    load_structure(In, DOM,
			   [ dialect(sgml),
			     dtd(DTD),
			     syntax_errors(quiet),
			     max_errors(-1)
			   ]),
	    close(In)),
	xpath(DOM, //table(@align=center), Table),
	xpath(Table, tbody/tr/th(text), 'Name'), !,
	xpath(Table, tbody/tr(self), Row),
	xpath(Row, td(1), NameCell),
	findall(Name, xpath(NameCell, a(text), Name), Names),
	list_to_disj(Names, Name),
	xpath(Row, td(2,text), On),
	xpath(Row, td(3), TypeCell),
	findall(Prim,
		( sub_term(Sub, TypeCell),
		  atom(Sub),
		  normalize_space(atom(Prim), Sub),
		  prim(Prim)),
		Types, RestTypes),
	findall(Type, xpath(TypeCell, a(text), Type), RestTypes),
	list_to_disj(Types, Type).

list_to_disj([One], One) :- !.
list_to_disj([H|T], H|Disj) :-
	list_to_disj(T, Disj).

prim(double).
prim(string).
prim(int).

