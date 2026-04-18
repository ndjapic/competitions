# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, k, m, i, a: int8;
	b: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	m := 0;
	for i := 1 to n do begin
		read(a);
		if a mod k = 0 then begin
			inc(m);
			b[m] := a div k;
		end;
	end;
	readln;

	for i := 1 to m-1 do write(b[i], ' ');
	writeln(b[m]);
end.

```
