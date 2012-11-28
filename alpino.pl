:- module(ods_alpino,
	  [ parse_label/3		% +Label, -Parsed, +Options
	  ]).
:- use_module(om).
:- use_module(library(option)).

user:file_search_path(alpino, Home) :-
	getenv('ALPINO_HOME', Home).

:- load_files(alpino(alpino), [silent(true), if(changed)]).

parse_label(Label, Trees, Options) :-
	atom_codes(Label, Codes),
	alpino_tokenize:tokenize(Codes, Tokens),
	om_annotate(Tokens, Tokens1, Options),
	alpino_parse(Tokens1, Parsed, []),
	alpino_result(tree(user(dt)), Parsed, Trees).

om_annotate([], [], _).
om_annotate([H|T0], Tokens, Options) :-
	postag(H, Tag, Options), !,
	format(atom(PT), '~W', [Tag, [spacing(standard),quoted(true)]]),
	Tokens = ['[', '@postag', PT, H, ']' | Rest],
	om_annotate(T0, Rest, Options).
om_annotate([H0|T0], [H|T], Options) :-
	downcase_atom(H0, H),
	H \== H0,
	option(lower(true), Options, true), !,
	om_annotate(T0, T, Options).
om_annotate([H|T0], [H|T], Options) :-
	om_annotate(T0, T, Options).

%%	postag(+Token, -Tag, +Options)
%
%	Define explicit postag for Token.

postag(OM, Tag, Options) :-
	om(OM, R),
	option(om(true), Options, true),
	Tag = meas_mod_noun(om(R),count,meas). % om instead of de/het: provenance
postag(miljoen, number(hoofd(pl_num)), _).
postag(miljard, number(hoofd(pl_num)), _).
postag(biljoen, number(hoofd(pl_num)), _).
