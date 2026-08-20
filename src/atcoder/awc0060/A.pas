program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, x, a, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x);

	ans := 0;
	for i := 1 to n do begin
		read(a);
		inc(ans, max(a-x, 0));
	end;
	readln;

	writeln(ans);
end.
