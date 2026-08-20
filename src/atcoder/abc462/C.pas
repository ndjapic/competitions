program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 300 * 1000;
var
	n, i, x, mn, ans: int32;
	y: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do begin
		read(x);
		readln(y[x]);
	end;

	ans := 0;
	mn := n+1;
	for x := 1 to n do
		if mn > y[x] then begin
			inc(ans);
			mn := y[x];
		end;

	writeln(ans);
end.
