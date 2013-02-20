:- module(kmeans,
	  [ k_means/4			% :Map, +Count, +Objects, -Clusters
	  ]).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(random)).

:- meta_predicate
	k_means(2, +, +, -),
	k_dist(2, +, +, -),
	k_mean(2, +, -).

/** <module> K-Means for clustering rectangles and points
*/

%%	k_means(:Map, +Count, +Objects, -Clusters) is det.

k_means(Map, Count, Objects, Clusters) :-
	length(Objects, Len),
	(   Count >= Len
	->  maplist(one_element_list, Objects, Clusters)
	;   randset(Count, Len, SelectionIndices),
	    n_select(SelectionIndices, Objects, Selection),
	    maplist(center(Map), Selection, KM1),
	    MaxIter is max(10, log(Len)*5),
	    k_iterate(Map, 1, MaxIter, KM1, Objects, [], Clusters)
	).

k_iterate(Map, Iteration, MaxIter, Centroites, Objects, Old, Clusters) :-
	maplist(length, Old, OldS),
	debug(kmean, 'Clustering ~d ... ~p', [Iteration, OldS]),
	k_cluster(Map, Centroites, Objects, Clusters0),
	(   Clusters0 == Old
	->  Clusters = Old
	;   partition(==([]), Clusters0, Empty, NonEmpty),
	    fill_empty(Empty, NonEmpty, Clusters1),
	    Iteration2 is Iteration+1,
	    (	Iteration2 < MaxIter
	    ->  maplist(k_mean(Map), Clusters1, NewCentroites),
		k_iterate(Map, Iteration2, MaxIter, NewCentroites,
			  Objects, Clusters1, Clusters)
	    ;	Clusters = Clusters1
	    )
	).

%%	fill_empty(+Empty, +NonEmpty, -Clusters)
%
%	If we end up with empty clusters, take some random element
%	from the remaining clusters to fill them up.

fill_empty([], Clusters, Clusters).
fill_empty([_|T], Clusters0, Clusters) :-
	repeat,
	random_select(Cluster, Clusters0, Clusters1),
	Cluster = [_,_|_], !,
	random_select(Obj, Cluster, RestCluster),
	fill_empty(T, [[Obj],RestCluster|Clusters1], Clusters).


%%	new_centroites(+Count, :Map, +Clusters, -Centroites) is det.
%
%	Sometimes, clusters get empty.  This   predicate  introduces new
%	centroites by taking a random corner of random clusters.

new_centroites(0, _, _, []) :- !.
new_centroites(N, Map, Clusters, [Centroit|T]) :-
	random_select(Cluster, Clusters, Rest),
	maplist(rect(Map), Cluster, Rects),
	rect_union_list(Rects, Rect),
	random_corner(Rect, Centroit),
	N2 is N - 1,
	new_centroites(N2, Map, Rest, T).

random_corner(rect(Xs,Ys,Xe,Ye), Point) :-
	random_member(Point,
		      [ point(Xs,Ys),
			point(Xs,Ye),
			point(Xe,Ys),
			point(Xe,Ye)
		      ]).

k_cluster(Map, Centroites, Objects, Clusters) :-
	CTerm =.. [c|Centroites],
	functor(CTerm, _, Arity),
	length(Empty, Arity),
	maplist(=([]), Empty),
	ClusterTerm =.. [c|Empty],
	k_cluster_t(Objects, Map, CTerm, ClusterTerm),
	ClusterTerm =.. [_|Clusters].

k_cluster_t([], _, _, _).
k_cluster_t([H|T], Map, Centroites, Clusters) :-
	center(Map, H, CH),
	closest_centroit(CH, Centroites, I),
	arg(I, Clusters, Cluster),
	setarg(I, Clusters, [H|Cluster]),
	k_cluster_t(T, Map, Centroites, Clusters).

closest_centroit(C0, Centroites, I) :-
	functor(Centroites, _, Arity),
	arg(Arity, Centroites, CBest),
	pt_distance(CBest, C0, DBest),
	A2 is Arity-1,
	closest_centroit(C0, A2, Centroites, DBest, Arity, I).

closest_centroit(_, 0, _, _, I, I) :- !.
closest_centroit(C0, A, Centroites, DBest, I0, I) :-
	arg(A, Centroites, C),
	pt_distance(C0, C, D),
	(   D < DBest
	->  I1 = A,
	    DBest1 = D
	;   I1 = I0,
	    DBest1 = DBest
	),
	A2 is A - 1,
	closest_centroit(C0, A2, Centroites, DBest1, I1, I).


pt_distance(point(X1,Y1), point(X2,Y2), D) :-
	D is sqrt((X2-X1)**2+(Y2-Y1)**2).


one_element_list(Obj, [Obj]).

n_select(Indices, Set, Selection) :-
	n_select(Indices, 1, Set, Selection).

n_select([], _, _, []) :- !.
n_select([I|IT], I, [H|T0], [H|T]) :- !,
	I2 is I+1,
	n_select(IT, I2, T0, T).
n_select(IL, I0, Set0, Set) :-
	I1 is I0+1,
	n_select(IL, I1, Set0, Set).

center(Map, Obj, point(X,Y)) :-
	rect(Map, Obj, rect(Xs,Ys, Xe,Ye)),
	X is (Xe+Xs)/2,
	Y is (Ye+Ys)/2.

%%	k_dist(+Map, +Object1, +Object2, -Distance) is det.
%
%	True when Distance is the Euclidean distance between Object1 and
%	Object2. This is defined as the length   of  the diagonal of the
%	minimum bounding box then contains both objects.

k_dist(Map, O1, O2, D) :-
	rect(Map, O1, R1),
	rect(Map, O2, R2),
	rect_union(R1, R2, rect(Xs,Ys, Xe,Ye)),
	D is sqrt((Xe-Xs)**2+(Ye-Ys)**2).

rect_union(rect(Xas,Yas, Xae,Yae),
	   rect(Xbs,Ybs, Xbe,Ybe),
	   rect(Xs,Ys, Xe,Ye)) :-
	Xs is min(Xas,Xbs),
	Xe is max(Xae,Xbe),
	Ys is min(Yas,Ybs),
	Ye is max(Yae,Ybe).

rect_union_list([H|T], Union) :- !,
	rect_union_list(T, H, Union).

rect_union_list([], Union, Union).
rect_union_list([H|T], Union0, Union) :-
	rect_union(H, Union0, Union1),
	rect_union_list(T, Union1, Union).

%%	k_mean(:Map, +Objects, -Mean) is det.
%
%	Is true if mean is the  centrum   of  gravity  of the objects in
%	List.
%
%	@param Mean is a term point(MX,MY).

k_mean(Map, Objects, point(X,Y)) :-
	assertion(Objects \== []),
	maplist(rect(Map), Objects, Rects),
	maplist(area, Rects, Areas),
	sum_xy(Rects, Areas, XSum, YSum),
	sum_list(Areas, Den),
	X is XSum/Den,
	Y is YSum/Den.

sum_xy(Rects, Areas, XSum, YSum) :-
	sum_xy(Rects, Areas, 0, XSum, 0, YSum).

sum_xy([], [], XSum, XSum, YSum, YSum).
sum_xy([rect(Xs,Ys, Xe,Ye)|RT], [A|AT], XSum0, XSum, YSum0, YSum) :-
	XSum1 is XSum0 + A*((Xs+Xe)/2),
	YSum1 is YSum0 + A*((Ys+Ye)/2),
	sum_xy(RT, AT, XSum1, XSum, YSum1, YSum).

area(rect(Xs,Ys, Xe,Ye), A) :-
	A is Xe-Xs*Ye-Ys.

%%	rect(:Map, +Object, -Rect)
%
%	Rect is the bounding box of Object.  Rect is a term
%	rect(Xs,Ys, Xe,Ye).

rect(Map, O, Rect) :-
	call(Map, O, Rect).

