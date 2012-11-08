:- module(bisect,
	  [ bisect/4				% :Test, +Low, +High, -LowestFail
	  ]).

%%	bisect(:Test, +Low, +High, -LowestFail) is semidet.
%
%	True when LowestFail is the lowest  integer between Low and High
%	for which call(Test,LowestFail) fails.  Fails if call(Test,High)
%	succeeds.
%
%	This  predicate  assumes  that  there  is   a  Value  such  that
%	call(Test,X) is true for all X in [Low..Value) and false for all
%	X in [Value..High].

:- meta_predicate
	bisect(1, +, +, -).

bisect(Test, _, To, _) :-
	call(Test, To), !,
	fail.
bisect(Test, From, To, Last) :-
	bsect(Test, From, To, Last).

:- meta_predicate
	bsect(1, +, +, -).

bsect(Test, From, To, Last) :-
	Mid is (From+To)//2,
	(   call(Test, Mid)
	->  (   Mid+1 >= To
	    ->	Last = To
	    ;	bsect(Test, Mid, To, Last)
	    )
	;   (   Mid == From
	    ->	Last = From
	    ;	bsect(Test, From, Mid, Last)
	    )
	).
