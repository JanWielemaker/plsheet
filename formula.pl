:- module(ods_formula,
	  [ sheet_formula_groups/2,	% :Sheet, -Groups
	    generalize_formula/8	% +S0, +X0, +Y0, +F0, -S, -X, -Y, -F
	  ]).
:- use_module(library(record)).
:- use_module(library(clpfd)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(lists)).
:- use_module(ods_table).

:- record
	map(sheet,x,y).

:- meta_predicate
	sheet_formula_groups(:, -).

/** <module> Reason about formulas


*/

%%	sheet_formula_groups(:Sheet, -Groups) is det.

sheet_formula_groups(Sheet, Groups) :-
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
	maplist(make_groups, CandidateGroups, NestedGroups),
	append(NestedGroups, Groups).

make_groups([], []).
make_groups([F0|FT], [g(P,[F0|Matching])|GT]) :-
	generalize_formula(F0, P),
	partition(match_formula(P), FT, Matching, Rest),
	length(Matching, Matches),
	length(Rest, Left),
	debug(formula, '~p: ~D matches; ~D left', [P, Matches, Left]),
	make_groups(Rest, GT).

match_formula(P, F) :-
	\+ \+ P = F.


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

