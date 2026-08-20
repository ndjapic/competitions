program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 10;
var
	n, m, k, a, b, i: int8;
	c: array [1 .. nn] of int8;
	space: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	for a := 1 to n do c[a] := 0;

	space := false;
	for i := 1 to k do begin
		readln(a, b);
		inc(c[a]);
		if c[a] = m then begin
			if space then
				write(' ')
			else
				space := true;
			write(a);
		end;
	end;

	writeln;
end.
