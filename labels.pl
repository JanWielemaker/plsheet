:- module(ods_labels,
	  [ assert_labels/1		% +Sheet
	  ]).
:- use_module(ods_table).
:- use_module(data).

:- meta_predicate
	assert_labels(:).

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
