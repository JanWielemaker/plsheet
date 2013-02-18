:- module(rtree,
	  [ list_to_rtree/3			% :Map, +Objects, -RTree
	  ]).
:- use_module(kmeans).
:- use_module(library(apply)).

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

%%	rect(:Map, +Object, -Rect)
%
%	Rect is the bounding box of Object.  Rect is a term
%	rect(Xs,Ys, Xe,Ye).

rect(Map, O, Rect) :-
	call(Map, O, Rect).
