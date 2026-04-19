# Problem: D.pas

```pascal
program _D;
{$OPTIMIZATION LEVEL3,ON}
const
	nn = 300 * 1000;
var
	notc, tci, n, i: int32;
	a, f: array [1 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(f[i]); readln;

		for i := 2 to n-1 do a[i] := (f[i+1] + f[i-1]) div 2 - f[i];

		a[1] := f[n];
		a[n] := f[1];
		for i := 2 to n-1 do begin
			a[1] := a[1] - a[i] * (n-i);
			a[n] := a[n] - a[i] * (i-1);
		end;
		a[1] := a[1] div (n-1);
		a[n] := a[n] div (n-1);

		for i := 1 to n-1 do write(a[i], ' ');
		writeln(a[n]);

	end;
end.

```
