# Problem: D_Beautiful_Permutation.pas

```pascal
program D_Beautiful_Permutation;
{$MODE DELPHI}
var
	notc, tci, n, l, r, m, d, ld, rd: int32;
	loop: boolean;

function difference(l, r: int32): int32;
var
	x: int32;
begin
	writeln('2 ', l, ' ', r);
	flush(output);
	readln(Result);

	writeln('1 ', l, ' ', r);
	flush(output);
	readln(x);
	dec(Result, x);
end;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		l := 1;
		r := n;
		d := difference(l, r);
		loop := true;

		while loop and (l < r) do begin
			m := (l+r) div 2;
			ld := difference(l, m);
			rd := d - ld;
			if ld = 0 then begin
				d := rd;
				l := m+1;
			end else if rd = 0 then begin
				d := ld;
				r := m;
			end else begin
				l := m+1-ld;
				r := m+rd;
				loop := false;
			end;
		end;

		writeln('! ', l, ' ', r);
		flush(output);

	end;
end.

```
