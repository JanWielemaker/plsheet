:- module(finance,
	  [ pmt/6			% +Rate, +Nper, +Pv, +Fv, +PayType, -Value
	  ]).

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
