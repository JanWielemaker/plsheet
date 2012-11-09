:- module(ods_alpino,
	  [ parse_label/2		% +Label, -Parsed
	  ]).

user:file_search_path(alpino, Home) :-
	getenv('ALPINO_HOME', Home).

:- load_files(alpino(alpino), [silent(true), if(changed)]).

parse_label(Label, Parsed) :-
	atom_codes(Label, Codes),
	alpino_tokenize:tokenize(Codes, Tokens),
	alpino_parse(Tokens, Parsed, []).
