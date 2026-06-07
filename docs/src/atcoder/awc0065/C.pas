program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp
uses
	math;
const
	nn = 200 * 1000;
var
	n, i, a: int32;
	dp0, dp1: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	dp0[0] := 0;
	dp1[1] := 0;

	for i := 1 to n do begin
		read(a);
		dp0[i] := max(dp0[i-1], dp1[i-1]);
		dp1[i] := dp0[i-1] + a;
	end;
	readln;

	writeln(max(dp0[n], dp1[n]));
end.
