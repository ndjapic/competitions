program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, p, q, d, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, p, q);

	ans := p;
	for i := 1 to n do begin
		read(d);
		ans := min(ans, q + d)
	end;
	readln;

	writeln(ans);
end.
