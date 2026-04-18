# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 10;
var
	n, i, j: int8;
	a, c: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do c[i] := 0;

	for i := 1 to n do begin
		read(a[i]);
		if a[i] > 0 then inc(c[a[i]]);
	end;
	readln;

	i := 1;
	while (i <= n) and (c[i] < 2) do inc(i);

	if i <= n then
		writeln('No')
	else begin
		j := 0;
		for i := 1 to n do
			if a[i] = -1 then begin
				inc(j);
				while c[j] > 0 do inc(j);
				a[i] := j;
			end;

		writeln('Yes');
		for i := 1 to n-1 do write(a[i], ' ');
		writeln(a[n]);
	end;
end.

```
