# Задатак: B_Siga_ta_Kymata.pas

```pascal
program B_Siga_ta_Kymata;
{$MODE DELPHI}
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, k, mn, mx: int32;
	s, x: string;
	p: array [1 .. nn] of int32;
	l, r: array [1 .. 5] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		setlength(s, n);

		for i := 1 to n do begin
			read(p[i]);
			s[i] := '0';
			if p[i] = 1 then r[1] := i;
			if p[i] = n then r[2] := i;
		end;
		readln;

		readln(x);

		l[1] := 1;
		l[2] := 1;
		l[3] := r[1];
		l[4] := r[2];
		r[3] := n;
		r[4] := n;
		l[5] := min(r[1], r[2]);
		r[5] := max(r[1], r[2]);

		for k := 1 to 5 do begin
			mn := min(p[l[k]], p[r[k]]);
			mx := max(p[l[k]], p[r[k]]);

			for i := l[k] + 1 to r[k] - 1 do
				if (mn < p[i]) and (p[i] < mx) then
					s[i] := '1';
		end;

		i := 1;
		while (i <= n) and ((x[i] = '0') or (s[i] = '1')) do inc(i);

		if i <= n then
			writeln(-1)
		else begin
			writeln(k);
			for i := 1 to k do writeln(l[i], ' ', r[i]);
		end;

	end;
end.

```
