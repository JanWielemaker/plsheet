:- module(test_rtree,
	  [ show_rtree/1			% +Tree
	  ]).
:- use_module(library(pce)).
:- use_module(library(autowin)).

show_rtree(T) :-
	new(P, auto_sized_picture),
	scale(T, Scale),
	draw_tree(P, Scale, 1, T),
	send(P, open).

scale(r_tree(_, rect(Xs,Ys,Xe,Ye),_,_), Scale) :-
	Size is max(Xe-Xs, Ye-Ys),
	(   Size =:= 0
	->  Scale = 1
	;   Scale is 1000/Size
	).

draw_tree(P, Scale, Level, r_tree(Map, Rect, Type, Children)) :-
	draw_rect(P, Scale, Level, Rect),
	SubLevel is Level+1,
	(   Type == internal
	->  maplist(draw_tree(P, Scale, SubLevel), Children)
	;   maplist(draw_object(Map, P, Scale, SubLevel), Children)
	).

draw_object(Map, P, Scale, Level, Obj) :-
	rect(Map, Obj, Rect),
	draw_rect(P, Scale, Level, Rect).

draw_rect(P, Scale, Level, rect(Xs,Ys,Xe,Ye)) :-
	W is round(Scale*(Xe+1-Xs)),
	H is round(Scale*(Ye+1-Ys)),
	X is round(Scale*Xs),
	Y is round(Scale*Ys),
	send(P, display, new(B, box(W,H)), point(X,Y)),
	(   color(Level, Color)
	->  send(B, colour, Color)
	;   true
	).

color(1, blue) :- !.
color(2, red) :- !.
color(3, green) :- !.
color(L, C) :-
	L2 is L-3,
	color(L2, C).


%%	rect(:Map, +Object, -Rect)
%
%	Rect is the bounding box of Object.  Rect is a term
%	rect(Xs,Ys, Xe,Ye).

rect(Map, O, Rect) :-
	call(Map, O, Rect).
