program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
const
	nn = 100;
var
	n, q, i, x, b, b0: int8;
	c: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for b := 1 to n do c[b] := 0;

	for i := 1 to q do begin
		read(x);

		if x >= 1 then
			b0 := x
		else begin
			b0 := 1;
			for b := 1 to n do
				if c[b] < c[b0] then b0 := b;
		end;

		inc(c[b0]);
		write(b0);
		if i < q then write(' ');
	end;
	readln;
	writeln;
end.
