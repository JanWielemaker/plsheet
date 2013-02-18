:- module(rtree,
	  [ list_to_rtree/3,			% :Map, +Objects, -RTree
	    rtree_inside/4,			% :Map, +Tree, +Rect, -Obj
	    rtree_intersects/4			% :Map, +Tree, +Target, -Obj
	  ]).
:- use_module(kmeans).
:- use_module(library(apply)).
:- use_module(library(lists)).

:- meta_predicate
	list_to_rtree(2, +, -).

/** <module> A pure Prolog R-tree implementation
*/

rtree_setting(node_size, 4).

:- meta_predicate
	rect(2, +, -).

%%	list_to_rtree(:Map, +Objects, -RTree) is det.
%
%	Create an R-Tree from Objects.

list_to_rtree(Map, Objects, RTree) :-
	length(Objects, Count),
	rtree_setting(node_size, NodeSize),
	Count =< NodeSize, !,
	maplist(rect(Map), Objects, Rects),
	rect_union_list(Rects, Rect),
	RTree = r_tree(Rect, leaf, Objects).
list_to_rtree(Map, Objects, RTree) :-
	rtree_setting(node_size, Size),
	k_means(Map, Size, Objects, Clusters),
	maplist(list_to_rtree(Map), Clusters, SubTrees),
	maplist(rect(r_tree_rect), SubTrees, Rects),
	rect_union_list(Rects, Rect),
	RTree = r_tree(Rect, internal, SubTrees).

r_tree_rect(Tree, Rect) :-
	arg(1, Tree, Rect).

%%	rtree_inside(:Map, +RTree, +Rect, -Object) is nondet.
%
%	True when Object is an object that intersects with Rect

rtree_inside(Map, r_tree(Rect, Type, Children), Target, Object) :-
	rect_intersects(Rect, Target),
	(   Type == internal
	->  member(Child, Children),
	    rtree_inside(Map, Child, Target, Object)
	;   member(Object, Children),
	    rect(Map, Object, ObjRect),
	    rect_intersects(Target, ObjRect)
	).

%%	rtree_intersects(:Map, +RTree, +Obj1, -Object) is nondet.
%
%	True when Object is an object that intersects with Obj1

rtree_intersects(Map, RTree, TargetObj, Object) :-
	rect(Map, TargetObj, Rect),
	rtree_inside(Map, RTree, Rect, Object).


%%	rect_union_list(+RectList, -Union)

rect_union_list([H|T], Union) :- !,
	rect_union_list(T, H, Union).
rect_union_list([], rect(0,0,0,0)).

rect_union_list([], Union, Union).
rect_union_list([H|T], Union0, Union) :-
	rect_union(H, Union0, Union1),
	rect_union_list(T, Union1, Union).

rect_union(rect(Xas,Yas, Xae,Yae),
	   rect(Xbs,Ybs, Xbe,Ybe),
	   rect(Xs,Ys, Xe,Ye)) :-
	Xs is min(Xas,Xbs),
	Xe is max(Xae,Xbe),
	Ys is min(Yas,Ybs),
	Ye is max(Yae,Ybe).

%%	rect_intersects(+Rect1, +Rect2) is semidet.
%
%	True when Rect1 and Rect2 have a non-empty intersection.

rect_intersects(Rect1, Rect2) :-
	rect_intersection(Rect1, Rect2, _).

rect_intersection(rect(SX1,SY1, EX1,EY1),
		  rect(SX2,SY2, EX2,EY2),
		  rect(SX,SY, EX,EY)) :-
	range_intersect(SX1,EX1, SX2,EX2, SX,EX),
	range_intersect(SY1,EY1, SY2,EY2, SY,EY).

range_intersect(S1,E1, S2,E2, S,E) :-
	S is max(S1,S2),
	E is min(E1,E2),
	S =< E.

%%	rect(:Map, +Object, -Rect)
%
%	Rect is the bounding box of Object.  Rect is a term
%	rect(Xs,Ys, Xe,Ye).

rect(Map, O, Rect) :-
	call(Map, O, Rect).
