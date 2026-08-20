program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, a, b, ans: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := 0;
	for i := 1 to n do begin
		readln(a, b);
		if b > a then inc(ans);
	end;

	writeln(ans);
end.
