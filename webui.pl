:- module(webui,
	  [ server/1,			% ?Port
	    show/1,			% +Data
	    show/2,			% +Data, +Options
	    clear/0
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(dialog/http_dialog).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(data).
:- use_module(recognise).

:- http_handler(root(.), home, []).
:- http_handler(root('webui.css'), http_reply_file('webui.css', []), []).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).


home(_Request) :-
	reply_html_page(title('Spreadsheet analyzer'),
			[ \html_requires(root('webui.css')),
			  h1('Spreadsheet analyzer'),
			  \error_area,
			  \log_area([id(log)]),
			  \form_area([id(form)])
			]).

show(Data) :-
	show(Data, []).

show(Data, Options) :-
	log_html(log, \webshow(Data), Options).

clear :-
	log_html(log, '', [clear(true)]).

webshow(Data) -->
	html(h4('Showing ~p'-[Data])),
	web_portray(Data).

web_portray(Var) -->
	{ var(Var) }, !,
	html(p('Unbound variable')).
web_portray(cell_range(Sheet, SX,SY, EX,EY)) -->
	{ integer(SX), integer(SY), integer(EX), integer(EY) }, !,
	html(table(class(spreadsheet),
		   [ tr([td([])|\column_headers(SX,EX)])
		   | \table_rows(Sheet, SX,SY, EX,EY)
		   ])).
web_portray(cell(Sheet,X,Y)) -->
	web_portray(cell_range(Sheet, X,Y, X,Y)).
web_portray(table(_Id,_Type,_DS,_Headers,Union)) -->
	web_portray(Union).
web_portray(sheet(Sheet)) -->
	{ sheet_bb(user:Sheet, DS) }, !,
	web_portray(DS).
web_portray(List) -->
	{ is_list(List), !,
	  length(List, Len)
	},
	html(h2('List of ~D objects'-[Len])),
	web_portray_list(List).
web_portray(_) -->
	html(p('No rules to portray')).

web_portray_list([]) --> "".
web_portray_list([H|T]) -->
	webshow(H), !,
	web_portray_list(T).

%%	column_headers(SX, EX)// is det.
%
%	Produce the column headers

column_headers(SX,EX) -->
	{ SX =< EX,
	  column_name(SX, Name),
	  X2 is SX+1
	},
	html(th(class(colname), Name)),
	column_headers(X2, EX).
column_headers(_, _) --> [].


%%	table_rows(+Sheet, +SX,+SY, +EX,+EY)// is det.

table_rows(Sheet, SX,SY, EX,EY) -->
	{ SY =< EY, !,
	  Y2 is SY+1
	},
	html(tr([ th(class(rowname),SY)
		| \table_row(Sheet, SY, SX, EX)
		])),
	table_rows(Sheet, SX,Y2, EX,EY).
table_rows(_, _,_, _,_) --> [].

table_row(Sheet, Y, SX,EX) -->
	{ SX =< EX, !,
	  X2 is SX+1
	},
	table_cell(Sheet, SX,Y),
	table_row(Sheet, Y, X2,EX).
table_row(_, _, _,_) --> [].

%%	table_cell(+Sheet, +SX, +SY)//

table_cell(Sheet, SX, SY) -->
	{ (   cell_type(Sheet, SX,SY, Type)
	  ->  true
	  ;   Type = empty
	  ),
	  findall(A, cell_class_attr(Sheet,SX,SY,Type,A), Classes),
	  (   Classes == []
	  ->  Attrs = []
	  ;   Attrs = [class(Classes)]
	  )
	},
	table_cell(Type, Sheet, SX, SY, Attrs).

cell_class_attr(_, _, _, Type, Type).
cell_class_attr(Sheet, X, Y, _, Class) :-
	(   cell_property(Sheet, X, Y, objects(_ObjId1,_ObjId2))
	->  Class = intables
	;   cell_property(Sheet, X, Y, block(ObjId)),
	    (   object_property(ObjId, color(C))
	    ->  color_class(C, Class)
	    ;   Class = intable
	    )
	).
cell_class_attr(Sheet, X, Y, _, derived) :-
	cell_formula(Sheet, X, Y, _).

color_class(1, c1).
color_class(2, c2).
color_class(3, c3).
color_class(4, c4).


%%	table_cell(+Sheet, +SX, +SY, +Style)//

table_cell(percentage, Sheet, SX, SY, Attrs) -->
	{ cell_value(Sheet, SX,SY, Value),
	  Val is Value*100
	}, !,
	html(td(Attrs, ['~3f%'-[Val]])).
table_cell(float, Sheet, SX, SY, Attrs) -->
	{ cell_value(Sheet, SX,SY, Value),
	  number(Value),
	  ndigits(Value, 5, V2)
	}, !,
	html(td(Attrs, [V2])).
table_cell(_, Sheet, SX, SY, Attrs) -->
	{ cell_value(Sheet, SX,SY, Value)
	}, !,
	(   { atomic(Value) }
	->  html(td(Attrs, Value))
	;   html(td(Attrs, '~q'-[Value]))
	).
table_cell(_, _, _, _, Attrs) -->
	html(td(Attrs, [])).

ndigits(F0, _, F) :-
	F0 =:= 0, !,
	F = F0.
ndigits(F0, N, F) :-
	Times is 10**max(1,N-round(log10(abs(F0)))),
	F is round(F0*Times)/Times.
