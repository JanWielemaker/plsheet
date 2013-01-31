:- module(varnames,
	  [ name_variable/2,
	    variable_name/2
	  ]).

%%	name_variable(+Var, +Name) is det.
%
%	Assign a name to a variable. Succeeds   silently if Var is not a
%	variable (anymore).

name_variable(Var, Name) :-
	var(Var), !,
	put_attr(Var, variable_name, Name).
name_variable(_, _).

%%	variable_name(+Var, -Name) is semidet.
%
%	True if Var has been assigned Name.

variable_name(Var, Name) :-
	get_attr(Var, variable_name, Name).

variable_name:attr_unify_hook(_Name, _Var).
variable_name:attr_portray_hook(Name, _) :-
	write(Name).
variable_name:attribute_goals(_) --> [].
