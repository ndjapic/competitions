program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #mex #learn
const
	NN = 2000;
var
	n, i, x: int32;
	seen: array [0 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for x := 0 to NN do seen[x] := false;

	for i := 1 to n do begin
		read(x);
		seen[x] := true;
	end;
	readln;

	x := 0;
	while seen[x] do inc(x);
	writeln(x);
end.
