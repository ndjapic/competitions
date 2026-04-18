# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, m, i, j: int32;
	s, t: string;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);
	n := length(s);
	m := length(t);

	i := 1;
	for j := 1 to m do
		if s[i] = t[j] then begin
			a[i] := j;
			inc(i);
		end;

	for i := 1 to n-1 do write(a[i], ' ');
	writeln(a[n]);
end.

```
