:- module(gvugraph,
	  [ dotty_ugraph/1,		% +UGraph
	    dotty_ugraph/2		% +UGraph, :Map
	  ]).
:- use_module(library(settings)).
:- use_module(library(process)).
:- use_module(library(ugraphs)).
:- use_module(library(assoc)).

:- meta_predicate
	dotty_ugraph(+, 3).

:- setting(graphviz:dot_viewer, atom, xdot,
	   'Program to show dot graphs').

/** <module> Render Prolog ugraphs with graphviz
*/

%%	dotty_ugraph(+Graph) is det.
%%	dotty_ugraph(+Graph, :Map) is det.
%
%	Write dot representation to temporary file   and  open this file
%	using the dotty program. The closure  Map is called to determine
%	identifiers and properties of nodes and edges. It is called with
%	the following arguments:
%
%	  * call(:Map, +Attribute, node(Node), -Value)
%	  * call(:Map, +Attribute, edge(From,To), -Value)
%
%	If no Map is provided (dotty_ugraph/1), the   label of a node is
%	the output of print/1 on the node data.

dotty_ugraph(Graph) :-
	dotty_ugraph(Graph, no_attrs).
dotty_ugraph(Graph, Map) :-
	setup_call_cleanup(
	    tmp_file_stream(utf8, File, Out),
	    ugraph_to_dot(Out, Graph, Map),
	    close(Out)),
	setting(graphviz:dot_viewer, Program),
	thread_create(run_dotty(Program, File),
		      _,
		      [detached(true)]).

no_attrs(label, node(Node), Label) :-
	format(atom(Label), '~p', [Node]).

:- dynamic
	dotty_process/1.

run_dotty(Program, File) :-
	setup_call_cleanup(
	    ( process_create(path(Program), [File], [process(PID)]),
	      assert(dotty_process(PID))
	    ),
	    process_wait(PID, _),
	    ( retractall(dotty_process(PID)),
	      catch(delete_file(File), _, true)
	    )).

kill_dotties :-
	forall(dotty_process(PID),
	       process_kill(PID)).

:- at_halt(kill_dotties).

ugraph_to_dot(Out, UGraph, Map) :-
	vertices(UGraph, Vertices),
	edges(UGraph, Edges),
	phrase(ugraph(Vertices, Edges, Map), Codes),
	format(Out, 'digraph G {\n~s}\n', [Codes]).

ugraph(Vertices, Edges, Map) -->
	{ empty_assoc(IdMap0) },
	vertices(Vertices, 0, _, IdMap0, IdMap, Map),
	edges(Edges, IdMap, Map).

vertices([], _, _, IdMap, IdMap, _) --> [].
vertices([H|T], Id0, Id, IdMap0, IdMap, Map) -->
	{ node_id(H, HId, Id0, Id1, IdMap0, IdMap1, Map) },
	node(H, HId, Map),
	vertices(T, Id1, Id, IdMap1, IdMap, Map).

edges([], _, _) --> [].
edges([H|T], IdMap, Map) -->
	edge(H, IdMap, Map),
	edges(T, IdMap, Map).

node_id(N, NId, Id0, Id1, IdMap0, IdMap1, Map) :-
	(   call(Map, id, node(N), Id)
	->  NId = Id,
	    Id1 = Id0
	;   succ(Id0, Id1),
	    atomic_list_concat([n, Id0], NId)
	),
	put_assoc(N, IdMap0, NId, IdMap1).

node(Node, NodeID, Map) -->
	{ findall(a(Attr, Value, Type),
		  ( call(Map, Attr, node(Node), Value),
		    gv_attr(Attr, node, Type)
		  ),
		  Attrs),
	  Attrs \== []
	}, !,
	"  ", gv_id(NodeID), " [", gv_attrs(Attrs), "]\n".
node(_, _, _) -->
	[].

edge(From-To, IdMap, Map) -->
	{ get_assoc(From, IdMap, FromID),
	  get_assoc(To, IdMap, ToID),
	  findall(a(Attr, Value, Type),
		  ( call(Map, Attr, edge(From,To), Value),
		    gv_attr(Attr, edge, Type)
		  ),
		  Attrs)
	},
	"  ", gv_id(FromID), " -> ", gv_id(ToID),
	(   {Attrs \== []}
	->  " [", gv_attrs(Attrs), "]"
	;   ""
	),
	"\n".

gv_attrs([]) --> [].
gv_attrs([a(Name, Value, Type)|T]) -->
	gv_id(Name), "=", gv_value(Type, Value),
	(   {T==[]}
	->  []
	;   ", ",
	    gv_attrs(T)
	).

%%	gv_value(+Type, +Value)// is det.
%
%	Emit the value of an attribute.

gv_value(string, String) --> !,
	dcg_format('~w', [String]).
gv_value(int, Value) --> !,
	dcg_format('~d', [Value]).
gv_value(double, Value) --> !,
	dcg_format('~f', [Value]).
gv_value(bool, Value) --> !,
	dcg_format('~w', [Value]).
gv_value(color, Color) --> !,
	dcg_format('~w', [Color]).
gv_value(lblString, String) --> !,
	gv_value(escString, String).
gv_value(escString, String) --> !,
	gv_atom(String).
gv_value(_, Value) -->			% Warn?
	dcg_format('~w', [Value]).

%%	gv_id(+Id) //
%
%	Emit an identifier

gv_id(A) -->
	dcg_format('~w', [A]).

dcg_format(Format, Args, List, Tail) :-
	format(codes(List,Tail), Format, Args).

gv_atom(Atom) -->
	{ atom_codes(Atom, Codes) },
	"\"", cstring(Codes), "\"".

%%	cstring(+Codes)//
%
%	Create a C-string. Normally =dot=  appears   to  be  using UTF-8
%	encoding. Would there be a  safer   way  to  transport non-ascii
%	characters, such as \uXXXX?

cstring([]) -->
	[].
cstring([H|T]) -->
	(   cchar(H)
	->  []
	;   [H]
	),
	cstring(T).

cchar(0'\') --> "\\\'".			% ?
cchar(0'") --> "\\\"".
cchar(0'\n) --> "\\n".
cchar(0'\t) --> "\\t".
cchar(0'\b) --> "\\b".

%%	gv_attr(Attr, On, Type)

term_expansion(gv_attr(Name, OnSpec, Type),
	       Terms) :-
	findall(gv_attr(Name, On, Type),
		on(OnSpec, On),
		Terms).

on(String, On) :-
	sub_atom(String, _, 1, _, C),
	map_on(C, On).

map_on('E', edge).
map_on('N', node).
map_on('G', graph).
map_on('S', subgraph).
map_on('C', cluster_subgraph).

%%	gv_attr(?Attr, ?On, ?Type)
%
%	Generated using gv_attr/0 from gv_attrs.pl

gv_attr('Damping',	    'G',    double).
gv_attr('K',		    'GC',   double).
gv_attr('URL',		    'ENGC', escString).
gv_attr(area,		    'NC',   double).
gv_attr(arrowhead,	    'E',    arrowType).
gv_attr(arrowsize,	    'E',    double).
gv_attr(arrowtail,	    'E',    arrowType).
gv_attr(aspect,		    'G',    aspectType).
gv_attr(bb,		    'G',    rect).
gv_attr(bgcolor,	    'GC',   (color|colorList)).
gv_attr(center,		    'G',    bool).
gv_attr(charset,	    'G',    string).
gv_attr(clusterrank,	    'G',    clusterMode).
gv_attr(color,		    'ENC',  (color|colorList)).
gv_attr(colorscheme,	    'ENCG', string).
gv_attr(comment,	    'ENG',  string).
gv_attr(compound,	    'G',    bool).
gv_attr(concentrate,	    'G',    bool).
gv_attr(constraint,	    'E',    bool).
gv_attr(decorate,	    'E',    bool).
gv_attr(defaultdist,	    'G',    double).
gv_attr(dim,		    'G',    int).
gv_attr(dimen,		    'G',    int).
gv_attr(dir,		    'E',    dirType).
gv_attr(diredgeconstraints, 'G',    (string|bool)).
gv_attr(distortion,	    'N',    double).
gv_attr(dpi,		    'G',    double).
gv_attr(edgeURL,	    'E',    escString).
gv_attr(edgehref,	    'E',    escString).
gv_attr(edgetarget,	    'E',    escString).
gv_attr(edgetooltip,	    'E',    escString).
gv_attr(epsilon,	    'G',    double).
gv_attr(esep,		    'G',    (addDouble|addPoint)).
gv_attr(fillcolor,	    'NEC',  (color|colorList)).
gv_attr(fixedsize,	    'N',    bool).
gv_attr(fontcolor,	    'ENGC', color).
gv_attr(fontname,	    'ENGC', string).
gv_attr(fontnames,	    'G',    string).
gv_attr(fontpath,	    'G',    string).
gv_attr(fontsize,	    'ENGC', double).
gv_attr(forcelabels,	    'G',    bool).
gv_attr(gradientangle,	    'NCG',  int).
gv_attr(group,		    'N',    string).
gv_attr(headURL,	    'E',    escString).
gv_attr(head_lp,	    'E',    point).
gv_attr(headclip,	    'E',    bool).
gv_attr(headhref,	    'E',    escString).
gv_attr(headlabel,	    'E',    lblString).
gv_attr(headport,	    'E',    portPos).
gv_attr(headtarget,	    'E',    escString).
gv_attr(headtooltip,	    'E',    escString).
gv_attr(height,		    'N',    double).
gv_attr(href,		    'GCNE', escString).
gv_attr(id,		    'GNE',  escString).
gv_attr(image,		    'N',    string).
gv_attr(imagepath,	    'G',    string).
gv_attr(imagescale,	    'N',    bool).
gv_attr(label,		    'ENGC', lblString).
gv_attr(labelURL,	    'E',    escString).
gv_attr(label_scheme,	    'G',    int).
gv_attr(labelangle,	    'E',    double).
gv_attr(labeldistance,	    'E',    double).
gv_attr(labelfloat,	    'E',    bool).
gv_attr(labelfontcolor,	    'E',    color).
gv_attr(labelfontname,	    'E',    string).
gv_attr(labelfontsize,	    'E',    double).
gv_attr(labelhref,	    'E',    escString).
gv_attr(labeljust,	    'GC',   string).
gv_attr(labelloc,	    'NGC',  string).
gv_attr(labeltarget,	    'E',    escString).
gv_attr(labeltooltip,	    'E',    escString).
gv_attr(landscape,	    'G',    bool).
gv_attr(layer,		    'ENC',  layerRange).
gv_attr(layerlistsep,	    'G',    string).
gv_attr(layers,		    'G',    layerList).
gv_attr(layerselect,	    'G',    layerRange).
gv_attr(layersep,	    'G',    string).
gv_attr(layout,		    'G',    string).
gv_attr(len,		    'E',    double).
gv_attr(levels,		    'G',    int).
gv_attr(levelsgap,	    'G',    double).
gv_attr(lhead,		    'E',    string).
gv_attr(lheight,	    'GC',   double).
gv_attr(lp,		    'EGC',  point).
gv_attr(ltail,		    'E',    string).
gv_attr(lwidth,		    'GC',   double).
gv_attr(margin,		    'NCG',  (double|point)).
gv_attr(maxiter,	    'G',    int).
gv_attr(mclimit,	    'G',    double).
gv_attr(mindist,	    'G',    double).
gv_attr(minlen,		    'E',    int).
gv_attr(mode,		    'G',    string).
gv_attr(model,		    'G',    string).
gv_attr(mosek,		    'G',    bool).
gv_attr(nodesep,	    'G',    double).
gv_attr(nojustify,	    'GCNE', bool).
gv_attr(normalize,	    'G',    bool).
gv_attr(nslimit,	    'G',    double).
gv_attr(nslimit1,	    'G',    double).
gv_attr(ordering,	    'GN',   string).
gv_attr(orientation,	    'N',    double).
gv_attr(orientation,	    'G',    string).
gv_attr(outputorder,	    'G',    outputMode).
gv_attr(overlap,	    'G',    (string|bool)).
gv_attr(overlap_scaling,    'G',    double).
gv_attr(pack,		    'G',    bool).
gv_attr(packmode,	    'G',    packMode).
gv_attr(pad,		    'G',    (double|point)).
gv_attr(page,		    'G',    (double|point)).
gv_attr(pagedir,	    'G',    pagedir).
gv_attr(pencolor,	    'C',    color).
gv_attr(penwidth,	    'CNE',  double).
gv_attr(peripheries,	    'NC',   int).
gv_attr(pin,		    'N',    bool).
gv_attr(pos,		    'EN',   (point|splineType)).
gv_attr(quadtree,	    'G',    (quadType|bool)).
gv_attr(quantum,	    'G',    double).
gv_attr(rank,		    'S',    rankType).
gv_attr(rankdir,	    'G',    rankdir).
gv_attr(ranksep,	    'G',    (double|doubleList)).
gv_attr(ratio,		    'G',    double).
gv_attr(rects,		    'N',    rect).
gv_attr(regular,	    'N',    bool).
gv_attr(remincross,	    'G',    bool).
gv_attr(repulsiveforce,	    'G',    double).
gv_attr(resolution,	    'G',    double).
gv_attr(root,		    'GN',   (string|bool)).
gv_attr(rotate,		    'G',    int).
gv_attr(rotation,	    'G',    double).
gv_attr(samehead,	    'E',    string).
gv_attr(sametail,	    'E',    string).
gv_attr(samplepoints,	    'N',    int).
gv_attr(scale,		    'G',    (double|point)).
gv_attr(searchsize,	    'G',    int).
gv_attr(sep,		    'G',    (addDouble|addPoint)).
gv_attr(shape,		    'N',    shape).
gv_attr(shapefile,	    'N',    string).
gv_attr(showboxes,	    'ENG',  int).
gv_attr(sides,		    'N',    int).
gv_attr(size,		    'G',    (double|point)).
gv_attr(skew,		    'N',    double).
gv_attr(smoothing,	    'G',    smoothType).
gv_attr(sortv,		    'GCN',  int).
gv_attr(splines,	    'G',    bool).
gv_attr(start,		    'G',    startType).
gv_attr(style,		    'ENCG', style).
gv_attr(stylesheet,	    'G',    string).
gv_attr(tailURL,	    'E',    escString).
gv_attr(tail_lp,	    'E',    point).
gv_attr(tailclip,	    'E',    bool).
gv_attr(tailhref,	    'E',    escString).
gv_attr(taillabel,	    'E',    lblString).
gv_attr(tailport,	    'E',    portPos).
gv_attr(tailtarget,	    'E',    escString).
gv_attr(tailtooltip,	    'E',    escString).
gv_attr(target,		    'ENGC', escString).
gv_attr(tooltip,	    'NEC',  escString).
gv_attr(truecolor,	    'G',    bool).
gv_attr(vertices,	    'N',    pointList).
gv_attr(viewport,	    'G',    viewPort).
gv_attr(voro_margin,	    'G',    double).
gv_attr(weight,		    'E',    int).
gv_attr(width,		    'N',    double).
gv_attr(xlabel,		    'EN',   lblString).
gv_attr(xlp,		    'NE',   point).
gv_attr(z,		    'N',    double).
