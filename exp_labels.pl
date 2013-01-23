:- use_module(ods_table).
:- use_module(labels).

load :-
	ods_load('E-Design-v2.205 sept 2011.ods').

load_externals :-
	load_externals(user).

load_externals(M) :-
	findall(URL,
		( M:cell_formula(_,_,_,F),
		  sub_term(ext(URL,_), F)
		),
		URLs),
	sort(URLs, Unique),
	maplist(load_external, Unique).

load_external(URI) :-
	ods_current(_:URI), !.
load_external(URI) :-
	uri_file_name(URI, File),
	catch(ods_load(URI:File), E,
	      print_message(warning, E)),
	load_externals(URI).

assert_labels :-
	forall(ods_current(M:_),
	       assert_labels(M:_)).

labels_table :-
	findall(row(File, Label, Count),
		(   ods_current(M:URI),
		    uri_file_name(URI, Path),
		    file_base_name(Path, File),
		    writeln(M),
		    M:label(Label, Count)
		),
		Rows),
	csv_write_file('all_labels.cvs', Rows, []).

