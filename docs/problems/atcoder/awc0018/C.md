# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, q, i, j, l, r: int32;
	c, s: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);
	read(c[1]);
	s[1] := 0;

	for i := 2 to n do begin
		read(c[i]);
		s[i] := s[i-1];
		if c[i-1] = c[i] then inc(s[i]);
	end;
	readln;

	for j := 1 to q do begin
		readln(l, r);
		writeln(s[r] - s[l]);
	end;
end.

```
