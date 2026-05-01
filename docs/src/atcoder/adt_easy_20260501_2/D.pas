program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, m, i, j, l: int8;
	x: array [1 .. nn] of int8;
	taken: array [1 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for j := 1 to m do taken[j] := false;

	for i := 1 to n do begin
		readln(l);
		for j := 1 to l do read(x[j]);
		readln;

		j := 1;
		while (j <= l) and taken[x[j]] do inc(j);

		if j > l then
			writeln(0)
		else begin
			writeln(x[j]);
			taken[x[j]] := true;
		end;
	end;
end.
