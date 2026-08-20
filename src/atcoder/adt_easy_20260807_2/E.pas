program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 500 * 1000;
var
	n, i, a, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := 1;
	for i := 1 to n do begin
		read(a);

		if i <= ans then
			ans := max(ans, i + a - 1);
	end;
	readln;

	writeln(min(ans, n));
end.
