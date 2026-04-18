# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, m, i, j, x: int32;
	exhausted: boolean;
	s: int64;
	d, r: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, s);

	for i := 1 to n do begin
		read(d[i]);
		r[i] := 0;
	end;
	readln;

	for j := 1 to m do begin
		readln(i, x);
		r[i] := x;
	end;

	exhausted := false;
	for i := 1 to n do begin
		dec(s, d[i]);
		if exhausted then dec(s, d[i]);
		if not exhausted then exhausted := s <= 0;
		inc(s, r[i]);
	end;
	writeln(s);
end.

```
