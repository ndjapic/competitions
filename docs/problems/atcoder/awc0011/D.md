# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, q, i, j, p: int32;
	v: array [1 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do read(v[i]); readln;

	for i := 2 to n do begin
		read(p);
		inc(v[i], v[p]);
	end;
	readln;

	for j := 1 to q do begin
		readln(i);
		writeln(v[i]);
	end;
end.

```
