:- module(ods_formula,
	  [ sheet_formula_groups/3,	% :Sheet, -Groups, -Singles
	    generalize_formula/8	% +S0, +X0, +Y0, +F0, -S, -X, -Y, -F
	  ]).
:- use_module(library(record)).
:- use_module(library(clpfd)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(lists)).
:- use_module(ods_table).
:- use_module(varnames).

:- record
	map(sheet,x,y).

:- meta_predicate
	sheet_formula_groups(:, -).

/** <module> Reason about formulas


*/

%%	sheet_formula_groups(:Sheet, -Groups, -Singles) is det.

sheet_formula_groups(Sheet, Groups, Singles) :-
	findall(f(Sheet,X,Y,F),
		cell_formula(Sheet, X, Y, F),
		Formulas),
	length(Formulas, Count),
	debug(formula, '~q: Found ~D formulas', [Sheet, Count]),
	map_list_to_pairs(skolem_formula, Formulas, Keyed),
	keysort(Keyed, Sorted),
	group_pairs_by_key(Sorted, ByKey),
	pairs_values(ByKey, CandidateGroups),
	length(CandidateGroups, CCount),
	debug(formula, '~D candidate groups', [CCount]),
	maplist(make_groups, CandidateGroups, NestedGroups, NestedSingles),
	append(NestedGroups, Groups),
	append(NestedSingles, Singles).

make_groups([], [], []).
make_groups([F0|FT], Groups, Singles) :-
	generalize_formula(F0, P),
	partition(match_formula(P), FT, Matching, Rest),
	length(Matching, Matches),
	length(Rest, Left),
	debug(formula, '~p: ~D matches; ~D left', [P, Matches, Left]),
	(   Matching \== []
	->  make_group(P, [F0|Matching], G0),
	    Groups = [G0|GT],
	    RS = Singles
	;   Groups = GT,
	    Singles = [F0|RS]
	),
	make_groups(Rest, GT, RS).

match_formula(P, F) :-
	\+ \+ P = F.

%%	make_group(+Pattern, +Matches, -Group)
%
%	Turn a set of matches into a group.  Groups is a term
%	forall(Var in Values, Formula).
%
%	@param Pattern is a term f(S,X,Y,F), where S,X,Y are variables.
%	@param Matches is a list of ground terms f(S,X,Y,F).

make_group(P, Matches, Groups) :-
	P = f(S,X,Y,_),
	findall(b(S,X,Y), member(P,Matches), Bindings),
	maplist(arg(1), Bindings, AllSheets), sort(AllSheets, Sheets),
	maplist(arg(2), Bindings, AllXs),     sort(AllXs, Xs),
	maplist(arg(3), Bindings, AllYs),     sort(AllYs, Ys),
	group(Sheets, Xs, Ys, P, Matches, Groups).

group([S], [X],  Ys, f(S,X,Y,F), _, [forall(col,   Y in Ys, F)]) :- !,
	name_variable(X, 'X').
group([S], Xs,  [Y], f(S,X,Y,F), _, [forall(row,   X in Xs, F)]) :- !,
	name_variable(Y, 'Y').
group(Ss,  [X], [Y], f(S,X,Y,F), _, [forall(sheet, S in Ss, F)]) :- !.
group([S], Xs, Ys, P, Matches, Groups) :-
	P = f(S,X,Y,_),
	length(Xs, Xc),
	length(Ys, Yc),
	(   Xc < Yc
	->  findall(G, (member(X,Xs), make_group(P, Matches, G)), NGroups)
	;   findall(G, (member(Y,Ys), make_group(P, Matches, G)), NGroups)
	),
	append(NGroups, Groups).



%%	generalize_formula(F0, F) is det.
%%	generalize_formula(+S0, +X0, +Y0, +F0, -S, -X, -Y, -F) is det.
%
%	F is F0, after replacing coordinates by the variables X and Y or
%	constraints thereof. The idea is  that   F  now unifies to other
%	formulas that have the same  structure   with  the same relative
%	cell positions.

generalize_formula(f(S0,X0,Y0,F0), f(S,X,Y,F)) :-
	generalize_formula(S0, X0, Y0, F0, S, X, Y, F).

generalize_formula(S0, X0, Y0, F0, S, X, Y, F) :-
	Map = map(S0-S, X0-X, Y0-Y),
	generalize_formula(Map, F0, F).

skolem_formula(f(S0,X0,Y0,F0), F) :-
	generalize_formula(S0, X0, Y0, F0, 'S', 'X', 'Y', F).



generalize_formula(Map, cell(S0,X0,Y0), cell(S,X,Y)) :- !,
	generalize_sheet(S0, Map, S),
	generalize_x(X0, Map, X),
	generalize_y(Y0, Map, Y).
generalize_formula(Map,
		   cell_range(S0,SX0,SY0,EX0,EY0),
		   cell_range(S, SX, SY, EX, EY)) :- !,
	generalize_sheet(S0, Map, S),
	generalize_x(SX0, Map, SX),
	generalize_y(SY0, Map, SY),
	generalize_x(EX0, Map, EX),
	generalize_y(EY0, Map, EY).
generalize_formula(Map, From, To) :-
	compound(From), !,
	From =.. [Name|Args0],
	maplist(generalize_formula(Map), Args0, Args),
	To =.. [Name|Args].
generalize_formula(_, Formula, Formula).


generalize_sheet(S0, Map, S) :-
	map_sheet(Map, F-T),
	(   S0 == F
	->  S = T
	;   S = S0
	).
generalize_x(X0, Map, X) :-
	map_x(Map, F-T),
	generalize_cordinate(X0, F-T, X).
generalize_y(Y0, Map, Y) :-
	map_y(Map, F-T),
	generalize_cordinate(Y0, F-T, Y).

generalize_cordinate(X0, F-T, X) :-
	(   X0 == F
	->  X = T
	;   atom(T)
	->  X = T
	;   integer(X0)
	->  Dif is X0-F,
	    (	Dif > 0
	    ->	X #= T+Dif
	    ;	MinDif is -Dif,
		X #= T-MinDif
	    )
	;   X = X0
	).

