# Задатак: F_Minimize_Fixed_Points.pas

```pascal
program F_Minimize_Fixed_Points;
uses
	math;
const
	nn = 100 * 1000;
var
	ntc, tci, n, i, j, p, pp, x: int32;
	a, lpd: array [1 .. nn] of int32;

begin
	for n := 1 to nn do lpd[n] := n;

	p := 2;
	pp := p*p;
	while pp <= nn do begin

		if lpd[p] = p then
			while pp <= n do begin
				if lpd[pp] = pp then lpd[pp] := p;
				inc(pp, p);
			end;

		inc(p);
		pp := p*p;
	end;

	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		for i := 1 to n do a[i] := i;

		for i := 2 to n do begin
			j := i div lpd[i];
			if j > 1 then begin
				x := a[i];
				a[i] := a[j];
				a[j] := x;
			end;
		end;

		for i := 1 to n-1 do write(a[i], ' ');
		writeln(a[n]);

	end;
end.

```
