program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, k, a: int32;
	ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	ans := (n-1) div k;
	for i := 1 to n do begin
		read(a);
		inc(ans, a);
	end;
	readln;

	writeln(ans);
end.
