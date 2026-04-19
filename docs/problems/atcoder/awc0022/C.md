# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, m, k, t, i, b, l, r: int32;
	s: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k, t);

	for b := 0 to n do s[b] := 0;

	for i := 1 to m do begin
		read(b);
		s[b] := 1;
	end;
	readln;

	for b := 1 to n do inc(s[b], s[b-1]);

	for i := 1 to k do begin
		readln(l, r);
		if s[r] - s[l-1] >= t then
			writeln('YES')
		else
			writeln('NO');
	end;
end.

```
