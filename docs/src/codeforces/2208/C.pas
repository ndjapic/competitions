program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100 * 1000;
var
	notc, tci, n, i: int32;
	ans: extended;
	c, p: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do readln(c[i], p[i]);

		ans := 0.0;
		for i := n downto 1 do
			ans := max(ans, ans * (1 - p[i]/100) + c[i]);
		writeln(ans:0:7);

	end;
end.
