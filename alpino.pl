:- module(ods_alpino,
	  [ parse_label/2		% +Label, -Parsed
	  ]).
:- use_module(om).

user:file_search_path(alpino, Home) :-
	getenv('ALPINO_HOME', Home).

:- load_files(alpino(alpino), [silent(true), if(changed)]).

parse_label(Label, Trees) :-
	atom_codes(Label, Codes),
	alpino_tokenize:tokenize(Codes, Tokens),
	om_annotate(Tokens, Tokens1),
	alpino_parse(Tokens1, Parsed, []),
	alpino_result(tree(user(dt)), Parsed, Trees).

om_annotate([], []).
om_annotate([H|T0], Tokens) :-
	om(H, _R), !,
	Tokens = ['[', '@postag', 'meas_mod_noun(de,count,meas)', H, ']' | Rest],
	om_annotate(T0, Rest).
om_annotate([H|T0], [H|T]) :-
	om_annotate(T0, T).
