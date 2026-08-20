program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, m, i, a: int32;
	ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	ans := 0;
	for i := 1 to n do begin
		read(a);
		inc(ans, max(0, -a));
		inc(ans, max(0, a-m));
	end;
	readln;

	writeln(ans);
end.
