:- module(rtree,
	  [ empty_rtree/2,		% :Map, ?RTree
	    list_to_rtree/3,		% :Map, +Objects, -RTree
	    rtree_insert/3,		% +Object, +RTree0, -RTree
	    rtree_inside/3,		% +Tree, +Rect, -Obj
	    rtree_intersects/3		% +Tree, +Object, -Obj
	  ]).
:- use_module(kmeans).
:- use_module(library(apply)).
:- use_module(library(lists)).

:- meta_predicate
	empty_rtree(2, ?),
	list_to_rtree(2, +, -).

/** <module> A pure Prolog R-tree implementation
*/

rtree_setting(bulk_node_size, 2).
rtree_setting(split_node_size, 20).

:- meta_predicate
	rect(2, +, -).

%%	empty_rtree(:Map, ?Tree)
%
%	True when Tree is an empty RTree.

empty_rtree(Map, r_tree(Map, rect(0,0,0,0), leaf, [])).

%%	list_to_rtree(:Map, +Objects, -RTree) is det.
%
%	Create an R-Tree from Objects.

list_to_rtree(Map, Objects, RTree) :-
	length(Objects, Count),
	rtree_setting(bulk_node_size, NodeSize),
	Count =< NodeSize, !,
	maplist(rect(Map), Objects, Rects),
	rect_union_list(Rects, Rect),
	RTree = r_tree(Map, Rect, leaf, Objects).
list_to_rtree(Map, Objects, RTree) :-
	rtree_setting(bulk_node_size, Size),
	k_means(Map, Size, Objects, Clusters),
	maplist(list_to_rtree(Map), Clusters, SubTrees),
	maplist(rect(rtree_rect), SubTrees, Rects),
	rect_union_list(Rects, Rect),
	RTree = r_tree(Map, Rect, internal, SubTrees).

rtree_map(Tree, Map)   :- arg(1, Tree, Map).
rtree_rect(Tree, Rect) :- arg(2, Tree, Rect).

%%	rtree_insert(+Object, +RTreeIn, -RTreeOut) is det.
%
%	Insert Object into RTreeIn.

rtree_insert(Object, RTree0, RTree) :-
	Map = _:_,
	empty_rtree(Map, RTree0), !,		% only the root can be empty
	rect(Map, Object, Rect),
	RTree = r_tree(Map, Rect, leaf, [Object]).
rtree_insert(Object, RTree0, RTree) :-
	rtree_map(RTree0, Map),
	rect(Map, Object, Rect),
	rtree_insert(Rect, Object, RTree0, NewTrees),
	(   NewTrees = [One]
	->  RTree = One
	;   maplist(rect(rtree_rect), NewTrees, NewRects),
	    rect_union_list(NewRects, Union),
	    RTree = r_tree(Map, Union, internal, NewTrees)
	).

rtree_insert(Rect, Obj, RTree0, RTrees) :-
	rtree_insert_(Rect, Obj, RTree0, RTrees), !.
rtree_insert(Rect, Obj, RTree0, RTrees) :-
	gtrace,
	rtree_insert_(Rect, Obj, RTree0, RTrees).

rtree_insert_(Rect, Object,
	      r_tree(Map, Rect0, leaf, Children0),
	      NewTrees) :- !,
	Children1 = [Object|Children0],
	length(Children0, Count),
	rtree_setting(split_node_size, NodeSize),
	(   Count > NodeSize
	->  k_means(Map, 2, Children1, Clusters),
	    maplist(new_node(leaf, Map), Clusters, NewTrees)
	;   rect_union(Rect0, Rect, Rect1),
	    NewTrees = [r_tree(Map, Rect1, leaf, Children1)]
	).
rtree_insert_(Rect, Object,
	      r_tree(Map, Rect0, internal, Children0),
	      NewTrees) :-
	primary_child(Rect, Child, Children0),
	rtree_insert(Rect, Object, Child, New),
	selectchk(Child, Children0, Children1),
	rtree_insert_list(Children1, Rect, Object, Children2),
	append(New, Children2, Children3),
	length(Children3, Count),
	rtree_setting(split_node_size, NodeSize),
	(   Count > NodeSize
	->  k_means(rtree_rect, 2, Children3, Clusters),
	    maplist(new_node(internal, Map), Clusters, NewTrees)
	;   rect_union(Rect, Rect0, Union),
	    NewTrees = [r_tree(Map, Union, internal, Children3)]
	).

new_node(internal, Map, SubTrees, r_tree(Map, Rect, internal, SubTrees)) :-
	assertion(SubTrees \== []),
	maplist(rect(rtree_rect), SubTrees, Rects),
	rect_union_list(Rects, Rect).
new_node(leaf, Map, LeafNodes, r_tree(Map, Rect, leaf, LeafNodes)) :-
	maplist(rect(Map), LeafNodes, Rects),
	rect_union_list(Rects, Rect).

rtree_insert_list([], _, _, []).
rtree_insert_list([H|T0], Rect, Object, NewList) :-
	rtree_rect(H, Rect0),
	rect_intersects(Rect0, Rect), !,
	rtree_insert(Rect, Object, H, HList),
	append(HList, Rest, NewList),
	rtree_insert_list(T0, Rect, Object, Rest).
rtree_insert_list([H|T0], Rect, Object, [H|T]) :-
	rtree_insert_list(T0, Rect, Object, T).

primary_child(Rect, Child, [Child0|Children]) :-
	k_dist(Rect, Child0, DBest),
	primary_child(Children, Rect, Child0, Child, DBest).

primary_child([], _, Child, Child, _).
primary_child([H|T], Rect, Child0, Child, DBest) :-
	k_dist(Rect, H, Dist),
	(   Dist < DBest
	->  primary_child(T, Rect, H, Child, Dist)
	;   primary_child(T, Rect, Child0, Child, DBest)
	).

k_dist(Rect, Tree, D) :-
	rtree_rect(Tree, RectT),
	rect_union(Rect, RectT, rect(Xs,Ys, Xe,Ye)),
	D is sqrt((Xe-Xs)**2+(Ye-Ys)**2).


%%	rtree_inside(+RTree, +Rect, -Object) is nondet.
%
%	True when Object is an object that intersects with Rect

rtree_inside(r_tree(Map, Rect, Type, Children), Target, Object) :-
	rect_intersects(Rect, Target),
	(   Type == internal
	->  member(Child, Children),
	    rtree_inside(Child, Target, Object)
	;   member(Object, Children),
	    rect(Map, Object, ObjRect),
	    rect_intersects(Target, ObjRect)
	).

%%	rtree_intersects(+RTree, +Obj1, -Object) is nondet.
%
%	True when Object is an object that intersects with Obj1

rtree_intersects(RTree, TargetObj, Object) :-
	rtree_map(RTree, Map),
	rect(Map, TargetObj, Rect),
	rtree_inside(RTree, Rect, Object).

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
