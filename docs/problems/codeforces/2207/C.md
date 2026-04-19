# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 2000;
var
	notc, tci, n, h, i, d, d1, d2, l, r, m: int32;
	ans: int64;
	a: array [1 .. nn] of int32;
	b: array [1 .. nn, 1 .. nn] of int32;
	w: array [1 .. nn, 0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, h);

		for i := 1 to n do read(a[i]);
		readln;

		for d := 1 to n do begin
			b[d, d] := a[d];
			for i := d-1 downto 1 do b[d, i] := max(b[d, i+1], a[i]);
			for i := d+1 to n do b[d, i] := max(b[d, i-1], a[i]);
			w[d, 0] := 0;
			for i := 1 to n do w[d, i] := w[d, i-1] + h - b[d, i];
		end;

		ans := h - a[1];
		for d1 := 1 to n do begin
			for d2 := d1 + 1 to n do begin

				l := d1;
				r := d2 + 1;
				while r-l > 1 do begin
					m := (l+r) div 2;
					if b[d1, m] < b[d2, m] then
						l := m
					else
						r := m;
				end;

				ans := max(ans, w[d1, l] + w[d2, n] - w[d2, l]);
			end;
		end;

		writeln(ans);

	end;
end.

```
