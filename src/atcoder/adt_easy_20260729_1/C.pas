program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 50;
var
	n, m, i, j, x, y: int8;
	ans: int32;
	mood: array [1 .. NN, 1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for x := 1 to n do
		for y := 1 to n do mood[x, y] := false;

	for j := 1 to m do begin
		read(y);
		for i := 2 to n do begin
			read(x);
			mood[x, y] := true;
			mood[y, x] := true;
			y := x;
		end;
		readln;
	end;

	ans := 0;
	for x := 1 to n do
		for y := 1 to n do
			if mood[x, y] = false then inc(ans);

	writeln((ans - n) div 2);
end.
