program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unsolved
const
	NN = 100 * 1000;
var
	notc, tci, n, m, i, j, x, y: int32;
	ans: int64;
	a, b: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, m, x, y);

		for i := 1 to n do read(a[i]);
		readln;

		for j := 1 to m do read(b[j]);
		readln;



		writeln(ans);

	end;
end.
