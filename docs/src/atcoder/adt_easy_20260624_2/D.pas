program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, m, i, j, l: int32;
	x: array [1 .. NN] of int8;
	seen: array [1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for j := 1 to m do seen[j] := false;

	for i := 1 to n do begin
		read(l);
		for j := 1 to l do read(x[j]);
		readln;

		j := 1;
		while (j <= l) and seen[x[j]] do inc(j);
		if j > l then
			writeln(0)
		else begin
			seen[x[j]] := true;
			writeln(x[j]);
		end;
	end;
end.
