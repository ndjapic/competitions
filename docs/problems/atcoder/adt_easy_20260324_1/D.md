# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 10;
var
	n, i: int32;
	s: array [0 .. nn] of int32;
	a: array [1 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	s[0] := 0;

	for i := 1 to n do begin
		read(s[i]);
		a[i] := s[i] - s[i-1];
	end;
	readln;

	for i := 1 to n-1 do write(a[i], ' ');
	writeln(a[n]);
end.

```
