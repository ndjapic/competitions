# Задатак: A_Difficult_Contest.pas

```pascal
program A_Difficult_Contest;
{$MODE DELPHI}
var
	ntc, tci, n, l, r, cf, ct, cn: int32;
	s: string;

procedure makesafe();
begin
	while ct > 0 do begin
		s[l] := 'T';
		inc(l);
		dec(ct);
	end;

	while cf > 0 do begin
		s[l] := 'F';
		inc(l);
		dec(cf);
	end;

	while cn > 0 do begin
		s[l] := 'N';
		inc(l);
		dec(cn);
	end;

	inc(l);
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(s);
		n := length(s);

		l := 1;
		cf := 0;
		ct := 0;
		cn := 0;

		for r := 1 to n do
			case s[r] of
				'F': inc(cf);
				'T': inc(ct);
				'N': inc(cn);
				else makesafe();
			end;

		makesafe();
		writeln(s);

	end;
end.

```
