# Задатак: B.pas

```pascal
program _B;
{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, m, k, i, j, c: int32;
	s: int64;
	a, b: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	for i := 1 to n do read(a[i]); readln;

	c := 0;
	s := 0;
	for j := 1 to m do begin
		read(b[j]);
		i := b[j];

		if a[i] < k then begin
			inc(c);
			inc(s, a[i]);
		end;
	end;
	readln;

	writeln(c, ' ', s);
end.

```
