program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, l, r, x, y, ans: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, l, r);

	ans := 0;

	for i := 1 to n do begin
		readln(x, y);
		if (x <= l) and (r <= y) then inc(ans);
	end;

	writeln(ans);
end.
