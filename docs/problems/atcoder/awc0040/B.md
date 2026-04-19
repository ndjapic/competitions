# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, m, s, i, q, l, r: int32;
	b: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(m, s);
	q := s div m;

	for i := 1 to m do begin
		read(b[i]);
		inc(b[i], q);
	end;
	readln;

	b[0] := 0;
	for i := 1 to s mod m do inc(b[i]);
	for i := 1 to m do inc(b[i], b[i-1]);

	readln(n);
	for i := 1 to n do begin
		readln(l, r);
		writeln(b[r] - b[l-1]);
	end;
end.

```
