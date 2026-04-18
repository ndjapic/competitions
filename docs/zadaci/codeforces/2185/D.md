# Задатак: D.pas

```pascal
program D;
{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	notc, tci, n, m, h, i, l, r: int32;
	a, b, c: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, m, h);

		for i := 1 to n do read(a[i]);
		readln;

		l := 1;
		for r := 1 to m do begin
			readln(b[r], c[r]);
			inc(a[b[r]], c[r]);
			if a[b[r]] > h then
				while l <= r do begin
					dec(a[b[l]], c[l]);
					inc(l);
				end;
		end;

		for i := 1 to n-1 do write(a[i], ' ');
		writeln(a[n]);

	end;
end.

```
