:- module(ods_table,
	  [ ods_DOM/3,			% +File, -DOM, +Options
	    ods_load/1,			% :DOM
	    ods_clean/0,
	    ods_eval/2,			% +Expression, -Value
	    ods_style_property/2,	% :Style, ?Property
	    cell_style/4		% :Sheet, ?X, ?Y, ?Property
	  ]).
:- use_module(library(xpath)).
:- use_module(library(dcg/basics)).

:- meta_predicate
	ods_load(:),
	ods_eval(:, -),
	ods_style_property(:, ?),
	cell_style(:, ?, ?, ?).


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
	select_option(member(Member), Options, XMLOptions, 'content.xml'),
	archive_next_header(Archive, Member),
	setup_call_cleanup(
	    archive_open_entry(Archive, Stream),
	    load_structure(Stream, DOM, XMLOptions),
	    close(Stream)).

%%	ods_load(:DOM)
%
%	Convert tables in DOM into a  set   of  Prolog predicates in the
%	calling module.  The generated predicates are:
%
%	    - table(Name, Style)
%	    - col(Table, X, Style)
%	    - row(Table, Y, Style)
%	    - cell(Table, X, Y, Value, Type, Formula, Style, Annotation)
%	    - style(Style, Properties)

ods_load(Module:DOM) :-
	DOM = [element(_,_,_)], !,
	load_styles(DOM, Module),
	load_tables(DOM, Module).
ods_load(Module:File) :-
	ods_DOM(File, DOM, []),
	ods_load(Module:DOM).


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
	debug(ods(row), 'Processing row ~q', [Y]),
	forall(xpath(DOM, 'table:table-cell'(self), Cell),
	       load_cell(Cell, State, Module)),
	NextY is Y + 1,
	nb_setarg(2, State, NextY).

load_cell(DOM, State, Module) :-
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
	->  debug(ods(cell), '~w empty cells', [Rep])
	;   Content = [Annotation],
	    xpath(Annotation, /'office:annotation'(self), _)
	->  (   cell_style(DOM, Style),
	        cell_annotations(DOM, Annotations)
	    ->	forall(between(X0, End, X),
		       ( debug(ods(cell), '~q,~q: ~q', [X,Y,Value]),
			 assertz(Module:cell(Table,X,Y,
					     '',
					     no_type,
					     -,
					     Style,
					     Annotations))
		       ))
	    ;	print_message(warning, ods(convert_failed(cell, DOM)))
	    )
	;   (   cell_type(DOM, Type),
	        cell_style(DOM, Style),
		cell_value(DOM, Type, Value),
		cell_formula(DOM, Table, Formula),
		cell_annotations(DOM, Annotations)
	    ->  forall(between(X0, End, X),
		       ( debug(ods(cell), '~q,~q: ~q', [X,Y,Value]),
			 assertz(Module:cell(Table,X,Y,
					     Value,
					     Type,
					     Formula,
					     Style,
					     Annotations))
		       ))
	    ;	print_message(warning, ods(convert_failed(cell, DOM)))
	    )
	),
	NextX is End+1,
	nb_setarg(1, State, NextX).

cell_type(DOM, Type) :-
	xpath(DOM, /'table:table-cell'(@'office:value-type'), OfficeType),
	OfficeType = Type.

cell_style(DOM, Style) :-
	xpath(DOM, /'table:table-cell'(@'table:style-name'), Style), !.
cell_style(_, default).			% TBD: Use default column style

cell_value(DOM, Type, Value) :-
	xpath(DOM, /'table:table-cell'(@'office:value'), OfficeValue), !,
	convert_value(Type, OfficeValue, Value).
cell_value(DOM, date, Value) :-
	xpath(DOM, /'table:table-cell'(@'office:date-value'), OfficeValue), !,
	convert_date(OfficeValue, Value).
cell_value(DOM, string, Value) :-
	findall(T, xpath(DOM, 'text:p'(normalize_space), T), List),
	atomic_list_concat(List, Value).

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

convert_date(Text, date(Y,M,D)) :-
	atom_codes(Text, Codes),
	phrase(date(Y,M,D), Codes), !.
convert_date(Text, Text) :-
	print_message(warning, ods(convert_failed(date, Text))).

date(Y,M,D) -->
	integer(Y), "-", integer(M), "-", integer(D),
	{ between(1, 12, M),
	  between(1, 31, D)
	}.

%%	cell_annotations(+DOM, -Annotations:list) is det.

cell_annotations(DOM, Annotations) :-
	findall(Annot, cell_annotation(DOM, Annot), Annotations).

cell_annotation(DOM, Term) :-
	xpath(DOM, 'office:annotation'(self), Annotation),
	(   convert_annotation(Annotation, Term)
	->  true
	;   print_message(warning, ods(convert_failed(annotation, DOM)))
	).

convert_annotation(DOM, annotation(Date, Author, Text)) :-
	xpath(DOM, 'dc:date'(text), DateText),
	parse_time(DateText, Date),
	findall(T, xpath(DOM, 'text:p'(text), T), List),
	List = [Author|Rest],
	atomic_list_concat(Rest, Text).

%%	cell_formula(+DOM, +Table, -Formula) is det.

cell_formula(DOM, Table, Formula) :-
	xpath(DOM, /'table:table-cell'(@'table:formula'), OfficeFormula), !,
	(   compile_formula(OfficeFormula, Table, Formula)
	->  true
	;   print_message(warning, ods(convert_failed(formula, OfficeFormula))),
	    Formula = OfficeFormula
	).
cell_formula(_, _, -).


		 /*******************************
		 *	      STYLES		*
		 *******************************/

%%	load_styles(+DOM, +Module) is det.
%
%	Load the style information for the  spreadsheet. We simply store
%	the DOM content of the style,   leaving the high-level reasoning
%	to other predicates. One  advantage  of   this  is  that  we can
%	re-generate the style info.
%
%	@tbd	Styles defined here may refer to styles in =|styles.xml|=.

load_styles(DOM, Module) :-
	xpath(DOM, //'office:automatic-styles'(self), StylesDOM), !,
	forall(xpath(StylesDOM, 'style:style'(@'style:name' = Name), SDOM),
	       assertz(Module:style(Name, SDOM))).

%%	ods_style_property(:Style, ?Property) is nondet.
%
%	True when Property is a property of Style.
%
%	@see http://docs.oasis-open.org/office/v1.2/OpenDocument-v1.2-part1.html

ods_style_property(Module:Style, Property) :-
	Module:style(Style, DOM),
	(   nonvar(Property)
	->  once(style_property(Property, DOM))
	;   style_property(Property, DOM)
	).

style_property(font_weight(W), DOM) :-
	xpath(DOM, 'style:text-properties'(@'fo:font-weight'=W), _).
style_property(font_name(Name), DOM) :-
	xpath(DOM, 'style:text-properties'(@'style:font-name'=Name), _).
style_property(font_size(Size), DOM) :-
	xpath(DOM, 'style:text-properties'(@'fo:font-size'=Size0), _),
	convert_size(Size0, Size).
style_property(column_width(Size), DOM) :-
	xpath(DOM, 'table-column-properties'(@'style:column-width'=Size0), _),
	convert_size(Size0, Size).

convert_size(Atom, Term) :-
	size_suffix(Suffix),
	atom_concat(NumA, Suffix, Atom),
	atom_number(NumA, Num), !,
	Term =.. [Suffix,Num].
convert_size(Atom, Atom) :-
	print_message(warning, ods(unknown_size(Atom))).

size_suffix(pt).
size_suffix(cm).
size_suffix(mm).

%%	cell_style(:Sheet, ?X, ?Y, ?Style)
%
%	True when cell X,Y in Sheet has style property Style

cell_style(Module:Sheet, X, Y, Property) :-
	Module:cell(Sheet, X, Y, _V, _T, _F, Style, _A),
	ods_style_property(Style, Property).


		 /*******************************
		 *	      FORMULAS		*
		 *******************************/

%%	compile_formula(OfficeFormula, Table, Formula) is det.
%
%	Compile a formula into a  Prolog   expression.  Cells are of the
%	form cell(X,Y).
%
%	@see http://en.wikipedia.org/wiki/OpenFormula
%	@see http://docs.oasis-open.org/office/v1.2/OpenDocument-v1.2-part2.html

compile_formula(Text, Table, Formula) :-
	atom_codes(Text, Codes),
	phrase(formula(Formula, Table), Codes), !.

formula(Formula, Table) -->
	"of:=",
	expression(Formula, 1200, _, Table).

expression(Expr, Pri, RPri, Table) -->
	blanks,
	(   ods_number(Expr0)
	;   ods_string(Expr0)
%	;   ods_array(Expr0)
	;   ods_prefix_func(Expr0, Pri, RPri0, Table)
	;   "(", expression(Expr0, 1200, _, Table), ")"
	;   ods_function_call(Expr0, Table)
	;   ods_reference(Expr0, Table)
%	;   ods_quoted_label(Expr0)
%	;   ods_automatic_intersection(Expr0)
%	;   ods_named_expression(Expr0)
%	;   ods_error(Expr0)
	), blanks, !,
	{ var(RPri0) -> RPri0 = 0 ; true },
	ods_op_func(Expr0, Pri, RPri0, RPri, Expr, Table).

ods_prefix_func(Expr, Pri, OpPri, Table) -->
	ods_op(Op, prefix(OpPri, ArgPri)),
	{ OpPri =< Pri },
	expression(Expr0, ArgPri, _, Table),
	{ Expr =.. [Op,Expr0] }.

%%	ods_op_func(+LeftExpr, +MaxPri, +LeftExprPri, -Expr) is semidet.

ods_op_func(Left, Pri, PriL, RPri, Expr, Table) -->
	ods_op(Op, infix(OpPri, LeftPri, RightPri)),
	{ PriL =< LeftPri, OpPri =< Pri },
	expression(Right, RightPri, _, Table),
	{ Expr1 =.. [Op,Left,Right] },
	ods_op_func(Expr1, Pri, OpPri, RPri, Expr, Table).
ods_op_func(Left, Pri, PriL, RPri, Expr, Table) -->
	ods_op(Op, postfix(OpPri, LeftPri)),
	{ PriL =< LeftPri, OpPri =< Pri },
	{ Expr1 =.. [Op,Left] },
	ods_op_func(Expr1, Pri, OpPri, RPri, Expr, Table).
ods_op_func(Expr, _, Pri, Pri, Expr, _) -->
	"".

ods_op(Op, Type) -->
	ods_op(Op),
	{ ods_op(Op, Type) }.

ods_op(':') --> ":".
ods_op('!') --> "!".
ods_op('~') --> "~".
ods_op('+') --> "+".
ods_op('-') --> "-".
ods_op('%') --> "%".
ods_op('^') --> "^".
ods_op('*') --> "*".
ods_op('/') --> "/".
ods_op('&') --> "&".
ods_op('=') --> "=".
ods_op('<>') --> "<>".
ods_op('<=') --> "<=".
ods_op('<') --> "<".
ods_op('>=') --> ">=".
ods_op('>') --> ">".

ods_op(':', infix(10, 10, 9)).
ods_op('!', infix(20, 20, 19)).
ods_op('~', infix(30, 30, 29)).
ods_op('+', prefix(40, 40)).
ods_op('-', prefix(40, 40)).
ods_op('%', postfix(50, 50)).
ods_op('^', infix(60, 60, 59)).
ods_op('*', infix(70, 70, 69)).
ods_op('/', infix(70, 70, 69)).
ods_op('+', infix(80, 80, 79)).
ods_op('-', infix(80, 80, 79)).
ods_op('&', infix(90, 90, 89)).
ods_op('=', infix(100, 100, 99)).
ods_op('<>', infix(100, 100, 99)).
ods_op('<', infix(100, 100, 99)).
ods_op('<=', infix(100, 100, 99)).
ods_op('>', infix(100, 100, 99)).
ods_op('>=', infix(100, 100, 99)).

%%	ods_number(-Number)// is semidet.
%
%	Deal with numbers that start with . instead of 0.

ods_number(N) -->
	number(N), !.
ods_number(N) -->
	dot,
	digit(DF0),
	digits(DF),
	{ F = [0'0, 0'., DF0|DF] },
	(   exp
	->  int_codes(DI),
	    {E=[0'e|DI]}
	;   {E = ""}
	),
	{ append([F, E], Codes),
	  number_codes(N, Codes)
	}.

int_codes([C,D0|D]) -->
	sign(C), !,
	digit(D0),
	digits(D).
int_codes([D0|D]) -->
	digit(D0),
	digits(D).

sign(0'-) --> "-".
sign(0'+) --> "+".

dot --> ".".

exp --> "e".
exp --> "E".

%%	ods_string(-Atom)//

ods_string(String) -->
	"\"", str_codes(Codes), "\"",
	{ atom_codes(String, Codes) }.

str_codes([H|T]) -->
	str_code(H), !,
	str_codes(T).
str_codes([]) -->
	"".

str_code(0'") --> "\"\"", !.
str_code(C) --> [C], { C \== 0'" }.

%%	ods_function_call(Expr0)// is semidet.

ods_function_call(eval(Expr), Table) -->
	function_name(Name),
	blanks, "(", parameter_list(Args, Table),
	{ Expr =.. [Name|Args] }.

parameter_list([], _) -->
	")", !.
parameter_list([H|T], Table) -->
	expression(H, 1200, _, Table), !, blanks,
	(   ";"
	->  blanks,
	    parameter_list(T, Table)
	;   ")"
	->  { T = [] }
	).

function_name(Name) -->
	letter_xml(C0),
	function_name_codes(C),
	{ atom_codes(Name, [C0|C]) }.

function_name_codes([H|T]) -->
	function_name_code(H), !,
	function_name_codes(T).
function_name_codes([]) --> "".

function_name_code(C) -->
	[C],
	{ xml_base(C)
	; xml_digit(C)
	; C == 0'_
	; C == 0'.
	; xml_ideographic(C)
	; xml_combining(C)
	}, !.


letter_xml(C) --> [C], { xml_base(C) ;
			 xml_ideographic(C)
		       }, !.

%%	ods_reference(Expr0, Table)

ods_reference(Expr, Table) -->
	"[", reference(Expr, Table), "]", !.

reference(ext(IRI, Range), Table) -->
	"'", !, string(Codes), "'#",
	{ atom_codes(IRI, Codes) },
	range_address(Range, Table).
reference(Range, Table) -->
	range_address(Range, Table).
reference(error('#REF!'), _) -->
	"#REF!".

range_address(Ref, Table) -->
	sheet_locator_or_empty(Sheet, Table),
	".",
	(   cell(SX,SY)
	->  (   ":."
	    ->  cell(EX,EY),
		{ Ref = cell_range(Sheet, SX, SY, EX, EY) }
	    ;   { Ref = cell(Sheet, SX, SY) }
	    )
	;   column(Start)
	->  ":.",
	    column(End),
	    { Ref = col_range(Sheet, Start, End) }
	;   row(Start)
	->  ":.",
	    row(End),
	    { Ref = row_range(Sheet, Start, End) }
	).
range_address(Ref, _Table) -->
	sheet_locator(Sheet),
	".",
	(   cell(SX, SY)
	->  ":",
	    sheet_locator(Sheet2), cell(EX, EY),
	    { Ref = xcell_range(Sheet, SX, SY, Sheet2, EX, EY) }
	;   column(Start)
	->  ":",
	    sheet_locator(Sheet2), column(End),
	    { Ref = xcol_range(Sheet, Start, Sheet2, End) }
	;   row(Start)
	->  ":",
	    sheet_locator(Sheet2), row(End),
	    { Ref = xrow_range(Sheet, Start, Sheet2, End) }
	).

sheet_locator_or_empty(Sheet, _) -->
	sheet_locator(Sheet).
sheet_locator_or_empty(Table, Table) --> "".

sheet_locator(Sheet) -->
	sheet_name(Name),
	subtable_path(Name, Sheet).

subtable_path(Name, Locator) -->
	".",
	subtable_cell(One),
	{ Path0 = Name/One },
	subtable_path(Path0, Locator).
subtable_path(Path, Path) --> "".

subtable_cell(Cell) -->
	cell(Cell), !.
subtable_cell(Sheet) -->
	sheet_name(Sheet).

sheet_name(Name) -->
	( "$" ->  "" ; "" ),
	(   single_quoted(Name)
	;   sheet_name_code(C0),
	    sheet_name_codes(Codes)
	->  { atom_codes(Name, [C0|Codes]) }
	).

cell(cell(X,Y)) -->
	column(X),
	row(Y).

cell(X, Y) -->
	column(X),
	row(Y).

column(Col) -->
	( "$" ->  "" ; "" ),
	coln(0, Col0),
	{ Col is Col0+1 }.

coln(C0, C) -->
	[D],
	{ between(0'A, 0'Z, D), !,
	  C1 is C0*26+D-0'A
	},
	coln(C1, C).
coln(C, C) --> "".

row(Row) -->
	( "$" ->  "" ; "" ),
	rown(0, Row).

rown(R0, R) -->
	[D],
	{ between(0'0, 0'9, D), !,
	  R1 is R0*10+D-0'0
	},
	rown(R1, R).
rown(R, R) --> "".


%%	single_quoted(-Atom)

single_quoted(String) -->
	"'", sq_codes(Codes), "'",
	{ atom_codes(String, Codes) }.

sq_codes([H|T]) -->
	sq_code(H), !,
	sq_codes(T).
sq_codes([]) -->
	"".

sq_code(0'\') --> "''", !.
sq_code(C) --> [C], { C \== 0'\' }.

sheet_name_codes([H|T]) -->
	sheet_name_code(H), !,
	sheet_name_codes(T).
sheet_name_codes([]) --> "".

sheet_name_code(C) -->
	[C],
	{ \+ not_in_sheet_name(C) }.

not_in_sheet_name(0']).
not_in_sheet_name(0'.).
not_in_sheet_name(0'\s).
not_in_sheet_name(0'#).
not_in_sheet_name(0'$).


		 /*******************************
		 *    EXPRESSION EVALUATION	*
		 *******************************/

%%	ods_eval(:Expression, -Value) is det.
%
%	Evaluate an expression.

ods_eval(Module:Expression, Value) :-
	ods_eval(Expression, Value, Module).

ods_eval(cell(Sheet, X, Y), Value, M) :- !,
	(   M:cell(Sheet, X, Y, Value0, _Type, _, _, _)
	->  Value = Value0
	;   existence_error(cell, cell(Sheet, X, Y))
	).
ods_eval(cell_range(Sheet, SX,SY, EX,EY), List, M) :- !,
	(   SX =:= EX
	->  findall(V, (between(SY,EY,Y),
			ods_eval_if_exists(cell(Sheet,SX,Y), V, M)),
		    List)
	;   SY =:= EY
	->  findall(V, (between(SX,EX,X),
			ods_eval_if_exists(cell(Sheet,X,SY), V, M)),
		    List)
	;   print_message(warning, ods(eval(cell_range(Sheet, SX,SY, EX,EY))))
	).
ods_eval(eval(Expr), Value, M) :- !,
	eval_function(Expr, Value, M).
ods_eval(A+B, Value, M) :- !,
	ods_eval(A, VA, M),
	ods_eval(B, VB, M),
	Value is VA+VB.
ods_eval(A-B, Value, M) :- !,
	ods_eval(A, VA, M),
	ods_eval(B, VB, M),
	Value is VA-VB.
ods_eval(A*B, Value, M) :- !,
	ods_eval(A, VA, M),
	ods_eval(B, VB, M),
	Value is VA*VB.
ods_eval(A/B, Value, M) :- !,
	ods_eval(A, VA, M),
	ods_eval(B, VB, M),
	Value is VA/VB.
ods_eval(A=B, Value, M) :- !,
	ods_eval(A, VA, M),
	ods_eval(B, VB, M),
	(   VA == VB
	->  Value = @true
	;   Value = @false
	).
ods_eval('%'(A), Value, M) :- !,
	ods_eval(A, VA, M),
	(   VA >= 0, VA =< 100
	->  Value is VA/100.0
	;   domain_error(percentage, VA)
	).
ods_eval(X, X, _).

ods_evalm(M, Expr, Value) :-
	ods_eval(Expr, Value, M).


%%	ods_eval_if_exists(+Cell, -Value, +Module) is semidet.
%
%	Extract value for a cell if it exists.  Used for 'SUM'().

ods_eval_if_exists(cell(Sheet,X,Y), Value, M) :-
	M:cell(Sheet, X, Y, Value, _Type, _, _, _), !.

eval_function('IF'(Cond, Then, Else), Value, M) :- !,
	ods_eval(Cond, VC, M),
	(   VC == @true
	->  ods_eval(Then, Value, M)
	;   ods_eval(Else, Value, M)
	).
eval_function(Expr, Value, M) :-
	Expr =.. [Func|ArgExprs],
	maplist(ods_evalm(M), ArgExprs, Args),
	Expr1 =.. [Func|Args],
	(   eval(Expr1, Value)
	->  true
	;   print_message(warning, ods(eval(Expr1))),
	    Value = error(Expr1)
	).


eval('SUM'(List), Value) :-
	sum_list(List, Value).
eval('RANK'(V, List), Rank) :-
	msort(List, Sorted),
	reverse(Sorted, Descending),
	nth1(Rank, Descending, V).
eval('RANK'(V, List, Order), Rank) :-
	(   Order =:= 0
	->  eval('RANK'(V, List), Rank)
	;   msort(List, Ascending),
	    nth1(Rank, Ascending, V)
	).
eval('FALSE', @false).
eval('TRUE', @true).



		 /*******************************
		 *     XML CHARACTER CLASSES	*
		 *******************************/

xml_base(C) :- between(0x0041, 0x005A, C).
xml_base(C) :- between(0x0061, 0x007A, C).
xml_base(C) :- between(0x00C0, 0x00D6, C).
xml_base(C) :- between(0x00D8, 0x00F6, C).
xml_base(C) :- between(0x00F8, 0x00FF, C).
xml_base(C) :- between(0x0100, 0x0131, C).
xml_base(C) :- between(0x0134, 0x013E, C).
xml_base(C) :- between(0x0141, 0x0148, C).
xml_base(C) :- between(0x014A, 0x017E, C).
xml_base(C) :- between(0x0180, 0x01C3, C).
xml_base(C) :- between(0x01CD, 0x01F0, C).
xml_base(C) :- between(0x01F4, 0x01F5, C).
xml_base(C) :- between(0x01FA, 0x0217, C).
xml_base(C) :- between(0x0250, 0x02A8, C).
xml_base(C) :- between(0x02BB, 0x02C1, C).
xml_base(0x0386).
xml_base(C) :- between(0x0388, 0x038A, C).
xml_base(0x038C).
xml_base(C) :- between(0x038E, 0x03A1, C).
xml_base(C) :- between(0x03A3, 0x03CE, C).
xml_base(C) :- between(0x03D0, 0x03D6, C).
xml_base(0x03DA).
xml_base(0x03DC).
xml_base(0x03DE).
xml_base(0x03E0).
xml_base(C) :- between(0x03E2, 0x03F3, C).
xml_base(C) :- between(0x0401, 0x040C, C).
xml_base(C) :- between(0x040E, 0x044F, C).
xml_base(C) :- between(0x0451, 0x045C, C).
xml_base(C) :- between(0x045E, 0x0481, C).
xml_base(C) :- between(0x0490, 0x04C4, C).
xml_base(C) :- between(0x04C7, 0x04C8, C).
xml_base(C) :- between(0x04CB, 0x04CC, C).
xml_base(C) :- between(0x04D0, 0x04EB, C).
xml_base(C) :- between(0x04EE, 0x04F5, C).
xml_base(C) :- between(0x04F8, 0x04F9, C).
xml_base(C) :- between(0x0531, 0x0556, C).
xml_base(0x0559).
xml_base(C) :- between(0x0561, 0x0586, C).
xml_base(C) :- between(0x05D0, 0x05EA, C).
xml_base(C) :- between(0x05F0, 0x05F2, C).
xml_base(C) :- between(0x0621, 0x063A, C).
xml_base(C) :- between(0x0641, 0x064A, C).
xml_base(C) :- between(0x0671, 0x06B7, C).
xml_base(C) :- between(0x06BA, 0x06BE, C).
xml_base(C) :- between(0x06C0, 0x06CE, C).
xml_base(C) :- between(0x06D0, 0x06D3, C).
xml_base(0x06D5).
xml_base(C) :- between(0x06E5, 0x06E6, C).
xml_base(C) :- between(0x0905, 0x0939, C).
xml_base(0x093D).
xml_base(C) :- between(0x0958, 0x0961, C).
xml_base(C) :- between(0x0985, 0x098C, C).
xml_base(C) :- between(0x098F, 0x0990, C).
xml_base(C) :- between(0x0993, 0x09A8, C).
xml_base(C) :- between(0x09AA, 0x09B0, C).
xml_base(0x09B2).
xml_base(C) :- between(0x09B6, 0x09B9, C).
xml_base(C) :- between(0x09DC, 0x09DD, C).
xml_base(C) :- between(0x09DF, 0x09E1, C).
xml_base(C) :- between(0x09F0, 0x09F1, C).
xml_base(C) :- between(0x0A05, 0x0A0A, C).
xml_base(C) :- between(0x0A0F, 0x0A10, C).
xml_base(C) :- between(0x0A13, 0x0A28, C).
xml_base(C) :- between(0x0A2A, 0x0A30, C).
xml_base(C) :- between(0x0A32, 0x0A33, C).
xml_base(C) :- between(0x0A35, 0x0A36, C).
xml_base(C) :- between(0x0A38, 0x0A39, C).
xml_base(C) :- between(0x0A59, 0x0A5C, C).
xml_base(0x0A5E).
xml_base(C) :- between(0x0A72, 0x0A74, C).
xml_base(C) :- between(0x0A85, 0x0A8B, C).
xml_base(0x0A8D).
xml_base(C) :- between(0x0A8F, 0x0A91, C).
xml_base(C) :- between(0x0A93, 0x0AA8, C).
xml_base(C) :- between(0x0AAA, 0x0AB0, C).
xml_base(C) :- between(0x0AB2, 0x0AB3, C).
xml_base(C) :- between(0x0AB5, 0x0AB9, C).
xml_base(0x0ABD).
xml_base(0x0AE0).
xml_base(C) :- between(0x0B05, 0x0B0C, C).
xml_base(C) :- between(0x0B0F, 0x0B10, C).
xml_base(C) :- between(0x0B13, 0x0B28, C).
xml_base(C) :- between(0x0B2A, 0x0B30, C).
xml_base(C) :- between(0x0B32, 0x0B33, C).
xml_base(C) :- between(0x0B36, 0x0B39, C).
xml_base(0x0B3D).
xml_base(C) :- between(0x0B5C, 0x0B5D, C).
xml_base(C) :- between(0x0B5F, 0x0B61, C).
xml_base(C) :- between(0x0B85, 0x0B8A, C).
xml_base(C) :- between(0x0B8E, 0x0B90, C).
xml_base(C) :- between(0x0B92, 0x0B95, C).
xml_base(C) :- between(0x0B99, 0x0B9A, C).
xml_base(0x0B9C).
xml_base(C) :- between(0x0B9E, 0x0B9F, C).
xml_base(C) :- between(0x0BA3, 0x0BA4, C).
xml_base(C) :- between(0x0BA8, 0x0BAA, C).
xml_base(C) :- between(0x0BAE, 0x0BB5, C).
xml_base(C) :- between(0x0BB7, 0x0BB9, C).
xml_base(C) :- between(0x0C05, 0x0C0C, C).
xml_base(C) :- between(0x0C0E, 0x0C10, C).
xml_base(C) :- between(0x0C12, 0x0C28, C).
xml_base(C) :- between(0x0C2A, 0x0C33, C).
xml_base(C) :- between(0x0C35, 0x0C39, C).
xml_base(C) :- between(0x0C60, 0x0C61, C).
xml_base(C) :- between(0x0C85, 0x0C8C, C).
xml_base(C) :- between(0x0C8E, 0x0C90, C).
xml_base(C) :- between(0x0C92, 0x0CA8, C).
xml_base(C) :- between(0x0CAA, 0x0CB3, C).
xml_base(C) :- between(0x0CB5, 0x0CB9, C).
xml_base(0x0CDE).
xml_base(C) :- between(0x0CE0, 0x0CE1, C).
xml_base(C) :- between(0x0D05, 0x0D0C, C).
xml_base(C) :- between(0x0D0E, 0x0D10, C).
xml_base(C) :- between(0x0D12, 0x0D28, C).
xml_base(C) :- between(0x0D2A, 0x0D39, C).
xml_base(C) :- between(0x0D60, 0x0D61, C).
xml_base(C) :- between(0x0E01, 0x0E2E, C).
xml_base(0x0E30).
xml_base(C) :- between(0x0E32, 0x0E33, C).
xml_base(C) :- between(0x0E40, 0x0E45, C).
xml_base(C) :- between(0x0E81, 0x0E82, C).
xml_base(0x0E84).
xml_base(C) :- between(0x0E87, 0x0E88, C).
xml_base(0x0E8A).
xml_base(0x0E8D).
xml_base(C) :- between(0x0E94, 0x0E97, C).
xml_base(C) :- between(0x0E99, 0x0E9F, C).
xml_base(C) :- between(0x0EA1, 0x0EA3, C).
xml_base(0x0EA5).
xml_base(0x0EA7).
xml_base(C) :- between(0x0EAA, 0x0EAB, C).
xml_base(C) :- between(0x0EAD, 0x0EAE, C).
xml_base(0x0EB0).
xml_base(C) :- between(0x0EB2, 0x0EB3, C).
xml_base(0x0EBD).
xml_base(C) :- between(0x0EC0, 0x0EC4, C).
xml_base(C) :- between(0x0F40, 0x0F47, C).
xml_base(C) :- between(0x0F49, 0x0F69, C).
xml_base(C) :- between(0x10A0, 0x10C5, C).
xml_base(C) :- between(0x10D0, 0x10F6, C).
xml_base(0x1100).
xml_base(C) :- between(0x1102, 0x1103, C).
xml_base(C) :- between(0x1105, 0x1107, C).
xml_base(0x1109).
xml_base(C) :- between(0x110B, 0x110C, C).
xml_base(C) :- between(0x110E, 0x1112, C).
xml_base(0x113C).
xml_base(0x113E).
xml_base(0x1140).
xml_base(0x114C).
xml_base(0x114E).
xml_base(0x1150).
xml_base(C) :- between(0x1154, 0x1155, C).
xml_base(0x1159).
xml_base(C) :- between(0x115F, 0x1161, C).
xml_base(0x1163).
xml_base(0x1165).
xml_base(0x1167).
xml_base(0x1169).
xml_base(C) :- between(0x116D, 0x116E, C).
xml_base(C) :- between(0x1172, 0x1173, C).
xml_base(0x1175).
xml_base(0x119E).
xml_base(0x11A8).
xml_base(0x11AB).
xml_base(C) :- between(0x11AE, 0x11AF, C).
xml_base(C) :- between(0x11B7, 0x11B8, C).
xml_base(0x11BA).
xml_base(C) :- between(0x11BC, 0x11C2, C).
xml_base(0x11EB).
xml_base(0x11F0).
xml_base(0x11F9).
xml_base(C) :- between(0x1E00, 0x1E9B, C).
xml_base(C) :- between(0x1EA0, 0x1EF9, C).
xml_base(C) :- between(0x1F00, 0x1F15, C).
xml_base(C) :- between(0x1F18, 0x1F1D, C).
xml_base(C) :- between(0x1F20, 0x1F45, C).
xml_base(C) :- between(0x1F48, 0x1F4D, C).
xml_base(C) :- between(0x1F50, 0x1F57, C).
xml_base(0x1F59).
xml_base(0x1F5B).
xml_base(0x1F5D).
xml_base(C) :- between(0x1F5F, 0x1F7D, C).
xml_base(C) :- between(0x1F80, 0x1FB4, C).
xml_base(C) :- between(0x1FB6, 0x1FBC, C).
xml_base(0x1FBE).
xml_base(C) :- between(0x1FC2, 0x1FC4, C).
xml_base(C) :- between(0x1FC6, 0x1FCC, C).
xml_base(C) :- between(0x1FD0, 0x1FD3, C).
xml_base(C) :- between(0x1FD6, 0x1FDB, C).
xml_base(C) :- between(0x1FE0, 0x1FEC, C).
xml_base(C) :- between(0x1FF2, 0x1FF4, C).
xml_base(C) :- between(0x1FF6, 0x1FFC, C).
xml_base(0x2126).
xml_base(C) :- between(0x212A, 0x212B, C).
xml_base(0x212E).
xml_base(C) :- between(0x2180, 0x2182, C).
xml_base(C) :- between(0x3041, 0x3094, C).
xml_base(C) :- between(0x30A1, 0x30FA, C).
xml_base(C) :- between(0x3105, 0x312C, C).
xml_base(C) :- between(0xAC00, 0xD7A3, C).

xml_ideographic(C) :- between(0x4E00, 0x9FA5, C).
xml_ideographic(0x3007).
xml_ideographic(C) :- between(0x3021, 0x3029, C).

xml_combining(C) :- between(0x0300, 0x0345, C).
xml_combining(C) :- between(0x0360, 0x0361, C).
xml_combining(C) :- between(0x0483, 0x0486, C).
xml_combining(C) :- between(0x0591, 0x05A1, C).
xml_combining(C) :- between(0x05A3, 0x05B9, C).
xml_combining(C) :- between(0x05BB, 0x05BD, C).
xml_combining(0x05BF).
xml_combining(C) :- between(0x05C1, 0x05C2, C).
xml_combining(0x05C4).
xml_combining(C) :- between(0x064B, 0x0652, C).
xml_combining(0x0670).
xml_combining(C) :- between(0x06D6, 0x06DC, C).
xml_combining(C) :- between(0x06DD, 0x06DF, C).
xml_combining(C) :- between(0x06E0, 0x06E4, C).
xml_combining(C) :- between(0x06E7, 0x06E8, C).
xml_combining(C) :- between(0x06EA, 0x06ED, C).
xml_combining(C) :- between(0x0901, 0x0903, C).
xml_combining(0x093C).
xml_combining(C) :- between(0x093E, 0x094C, C).
xml_combining(0x094D).
xml_combining(C) :- between(0x0951, 0x0954, C).
xml_combining(C) :- between(0x0962, 0x0963, C).
xml_combining(C) :- between(0x0981, 0x0983, C).
xml_combining(0x09BC).
xml_combining(0x09BE).
xml_combining(0x09BF).
xml_combining(C) :- between(0x09C0, 0x09C4, C).
xml_combining(C) :- between(0x09C7, 0x09C8, C).
xml_combining(C) :- between(0x09CB, 0x09CD, C).
xml_combining(0x09D7).
xml_combining(C) :- between(0x09E2, 0x09E3, C).
xml_combining(0x0A02).
xml_combining(0x0A3C).
xml_combining(0x0A3E).
xml_combining(0x0A3F).
xml_combining(C) :- between(0x0A40, 0x0A42, C).
xml_combining(C) :- between(0x0A47, 0x0A48, C).
xml_combining(C) :- between(0x0A4B, 0x0A4D, C).
xml_combining(C) :- between(0x0A70, 0x0A71, C).
xml_combining(C) :- between(0x0A81, 0x0A83, C).
xml_combining(0x0ABC).
xml_combining(C) :- between(0x0ABE, 0x0AC5, C).
xml_combining(C) :- between(0x0AC7, 0x0AC9, C).
xml_combining(C) :- between(0x0ACB, 0x0ACD, C).
xml_combining(C) :- between(0x0B01, 0x0B03, C).
xml_combining(0x0B3C).
xml_combining(C) :- between(0x0B3E, 0x0B43, C).
xml_combining(C) :- between(0x0B47, 0x0B48, C).
xml_combining(C) :- between(0x0B4B, 0x0B4D, C).
xml_combining(C) :- between(0x0B56, 0x0B57, C).
xml_combining(C) :- between(0x0B82, 0x0B83, C).
xml_combining(C) :- between(0x0BBE, 0x0BC2, C).
xml_combining(C) :- between(0x0BC6, 0x0BC8, C).
xml_combining(C) :- between(0x0BCA, 0x0BCD, C).
xml_combining(0x0BD7).
xml_combining(C) :- between(0x0C01, 0x0C03, C).
xml_combining(C) :- between(0x0C3E, 0x0C44, C).
xml_combining(C) :- between(0x0C46, 0x0C48, C).
xml_combining(C) :- between(0x0C4A, 0x0C4D, C).
xml_combining(C) :- between(0x0C55, 0x0C56, C).
xml_combining(C) :- between(0x0C82, 0x0C83, C).
xml_combining(C) :- between(0x0CBE, 0x0CC4, C).
xml_combining(C) :- between(0x0CC6, 0x0CC8, C).
xml_combining(C) :- between(0x0CCA, 0x0CCD, C).
xml_combining(C) :- between(0x0CD5, 0x0CD6, C).
xml_combining(C) :- between(0x0D02, 0x0D03, C).
xml_combining(C) :- between(0x0D3E, 0x0D43, C).
xml_combining(C) :- between(0x0D46, 0x0D48, C).
xml_combining(C) :- between(0x0D4A, 0x0D4D, C).
xml_combining(0x0D57).
xml_combining(0x0E31).
xml_combining(C) :- between(0x0E34, 0x0E3A, C).
xml_combining(C) :- between(0x0E47, 0x0E4E, C).
xml_combining(0x0EB1).
xml_combining(C) :- between(0x0EB4, 0x0EB9, C).
xml_combining(C) :- between(0x0EBB, 0x0EBC, C).
xml_combining(C) :- between(0x0EC8, 0x0ECD, C).
xml_combining(C) :- between(0x0F18, 0x0F19, C).
xml_combining(0x0F35).
xml_combining(0x0F37).
xml_combining(0x0F39).
xml_combining(0x0F3E).
xml_combining(0x0F3F).
xml_combining(C) :- between(0x0F71, 0x0F84, C).
xml_combining(C) :- between(0x0F86, 0x0F8B, C).
xml_combining(C) :- between(0x0F90, 0x0F95, C).
xml_combining(0x0F97).
xml_combining(C) :- between(0x0F99, 0x0FAD, C).
xml_combining(C) :- between(0x0FB1, 0x0FB7, C).
xml_combining(0x0FB9).
xml_combining(C) :- between(0x20D0, 0x20DC, C).
xml_combining(0x20E1).
xml_combining(C) :- between(0x302A, 0x302F, C).
xml_combining(0x3099).
xml_combining(0x309A).

xml_digit(C) :- between(0x0030, 0x0039, C).
xml_digit(C) :- between(0x0660, 0x0669, C).
xml_digit(C) :- between(0x06F0, 0x06F9, C).
xml_digit(C) :- between(0x0966, 0x096F, C).
xml_digit(C) :- between(0x09E6, 0x09EF, C).
xml_digit(C) :- between(0x0A66, 0x0A6F, C).
xml_digit(C) :- between(0x0AE6, 0x0AEF, C).
xml_digit(C) :- between(0x0B66, 0x0B6F, C).
xml_digit(C) :- between(0x0BE7, 0x0BEF, C).
xml_digit(C) :- between(0x0C66, 0x0C6F, C).
xml_digit(C) :- between(0x0CE6, 0x0CEF, C).
xml_digit(C) :- between(0x0D66, 0x0D6F, C).
xml_digit(C) :- between(0x0E50, 0x0E59, C).
xml_digit(C) :- between(0x0ED0, 0x0ED9, C).
xml_digit(C) :- between(0x0F20, 0x0F29, C).


		 /*******************************
		 *	      CLEANUP		*
		 *******************************/

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
	retractall(M:cell(_,_,_,_,_,_,_,_)),
	retractall(M:style(_,_)).
