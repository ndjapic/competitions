# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, q, i, j, a, l, r: int32;
	s: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);
	s[0] := 0;

	for i := 1 to n do begin
		read(a);
		s[i] := s[i-1] + a;
	end;
	readln;

	for j := 1 to q do begin
		readln(l, r);
		writeln(s[r] - s[l-1]);
	end;
end.

```
