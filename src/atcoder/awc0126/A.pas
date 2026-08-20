program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int32;
	d, x, y: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d);

	ans := 0;
	for i := 1 to n do begin
		readln(x, y);
		if sqr(x) + sqr(y) > sqr(d) then inc(ans);
	end;

	writeln(ans);
end.
