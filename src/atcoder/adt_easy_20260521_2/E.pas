program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, a, b, mx: int32;
	ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := 0;
	mx := 0;
	for i := 1 to n do begin
		readln(a, b);
		inc(ans, a);
		mx := max(mx, b-a);
	end;

	writeln(ans + mx);
end.
