:- module(om,
	  [ om/2
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).

:- rdf_register_prefix(om, 'http://www.wurvoc.org/vocabularies/om-1.8/').
:- rdf_load('OMVocabulary.ttl').


om(Symbol, Resource) :-
	rdf(Resource, om:symbol, literal(Symbol)).
