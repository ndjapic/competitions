# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i, h, mx, k: int32;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	mx := 0;
	k := 0;
	for i := 1 to n do begin
		read(h);
		if h > mx then begin
			inc(k);
			a[k] := i;
			mx := h;
		end;
	end;
	readln;

	for i := 1 to k-1 do write(a[i], ' ');
	writeln(a[k]);
end.

```
