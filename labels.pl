:- module(ods_labels,
	  [ assert_labels/1		% +Sheet
	  ]).
:- use_module(ods_table).
:- use_module(data).

:- meta_predicate
	assert_labels(:).


		 /*******************************
		 *	   FINDING LABELS	*
		 *******************************/

%%	assert_labels(?Sheet)
%
%	Collect all labels. Each label is represented as a label(Text,
%	Count).

assert_labels(Sheet) :-
	Sheet = M:SheetName,
	findall(Label,
		( M:sheet(SheetName, _Style),
		  cell_type(Sheet,X,Y,string),
		  cell_value(Sheet,X,Y,Label),
		  Label \== ''
		),
		Labels),
	msort(Labels, Sorted),
	count(Sorted, CountLabel),
	keysort(CountLabel, ByCount),
	reverse(ByCount, ByCountDown),
	forall(member(Count-Label, ByCountDown),
	       assert_label(M:Label, Count)).

count([], []).
count([H|T0], [C-H|T]) :-
	same(H, 1, C, T0, T1),
	count(T1, T).

same(H, C0, C, [H|T0], T) :- !,
	C1 is C0+1,
	same(H, C1, C, T0, T).
same(_, C, C, T, T).

%%	repeating_pattern(+List, -RepDesc) is det.
%
%	Represent List fragments using repeat(Count, Seq).  E.g.
%
%	  ==
%	  ?- repeating_pattern([a,b,c,b,c,d], P).
%	  P is [repeat(1,[a]), repeat(2,[b,c]), repeat(1,[d])].
%	  ==

repeating_pattern([], []) :- !.
repeating_pattern(List, [repeat(Rep,Pat0)|Pattern]) :-
	rappend(Pat0, Rest0, List),
	Pat0 \== [],
	append(Pat0, Rest1, Rest0), !,
	repeat(Pat0, Rest1, 2, Rep, Rest1),
	repeating_pattern(Rest1, Pattern).
repeating_pattern([H|T], [repeat(1,H)|Pattern]) :-
	repeating_pattern(T, Pattern).

repeat(Pat, List, Rep0, Rep, Rest) :-
	append(Pat, Rest0, List), !,
	Rep1 is Rep0+1,
	repeat(Pat, Rest0, Rep1, Rep, Rest).
repeat(_, List, Rep, Rep, List).

rappend([H|T], L, [H|R]) :-
	rappend(T, L, R).
rappend([], L, L).
