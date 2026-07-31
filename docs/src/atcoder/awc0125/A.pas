program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, d, s, t: int32;
	ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d, s);

	ans := int64(n-1) * d;
	for i := 1 to n do begin
		read(t);
		inc(ans, t);
	end;
	readln;

	inc(ans, int64(d) * min(s-1, n-s));
	writeln(ans);
end.
