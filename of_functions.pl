:- module(of_functions,
	  [ pmt/6			% +Rate, +Nper, +Pv, +Fv, +PayType, -Value
	  ]).

/** <module> Advanced Open Formula functions

This module provides the more  advanced   functions  defined by the Open
Formula specification.

@tbd	Implement most of them
@see	http://cgit.freedesktop.org/libreoffice/core/tree/sc/source/core/tool/interpr2.cxx
*/

%%	pmt(+Zins, +Zzr, +Bw, +Zw, +F, -Value)
%
%	@see http://docs.oasis-open.org/office/v1.2/os/OpenDocument-v1.2-os-part2.html#PMT
%	@see http://wiki.openoffice.org/wiki/Documentation/How_Tos/Calc:_PMT_function

pmt(Zins, Zzr, Bw, Zw, _, Value) :-
	Zins =:= 0.0, !,
	Rmz is (Bw+Zw)/Zzr,
	Value is -Rmz.
pmt(Zins, Zzr, Bw, Zw, F, Value) :-
	Term is (1.0+Zins)**Zzr,
	(   F > 0.0
	->  Rmz is (Zw*Zins/(Term-1.0)
		      + Bw*Zins/(1.0-1.0/Term)) / (1.0+Zins)
	;   Rmz is Zw*Zins/(Term-1.0)
	             + Bw*Zins/(1.0-1.0/Term)
	),
	Value is -Rmz.
