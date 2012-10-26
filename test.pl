:- use_module(ods_table).

file('E-Design WindEnergie.ods').

dom(DOM) :-
	file(File),
	ods_DOM(File, DOM, []).

load :-
	file(File),
	ods_clean,
	ods_load(File).
