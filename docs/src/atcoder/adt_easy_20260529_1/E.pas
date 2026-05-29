program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100 * 1000;
var
	n, i, a: int32;
	space: boolean;
	c: array [1 .. 3*NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for a := 1 to n do c[a] := 0;
	space := false;

	for i := 1 to 3*n do begin
		read(a);
		inc(c[a]);
		if c[a] = 2 then begin
			if space then
				write(' ')
			else
				space := true;
			write(a);
		end;
	end;
	readln;
	writeln;
end.
