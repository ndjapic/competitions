program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #stack #dictionary #dp
uses
	math;
const
	NN = 200 * 1000;
	MM = 1000 * 1000;
var
	n, i, a, d: int32;
	dp: array [1 .. MM] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for a := 1 to MM do dp[a] := 0;

	readln(n);
	d := n;

	for i := 1 to n do begin
		read(a);
		if dp[a] > 0 then
			d := min(d, i - dp[a]);
		dp[a] := i;
	end;
	readln;

	if d = n then
		writeln(-1)
	else
		writeln(d + 1);
end.
