# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, m, i, j, t, s, l, r: int32;
	pre: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	pre[0] := 0;
	for i := 1 to n do begin
		read(t);
		pre[i] := pre[i-1] + t;
	end;
	readln;

	for j := 1 to m do begin
		readln(s, l, r);
		writeln(s + pre[r] - pre[l-1]);
	end;
end.

```
