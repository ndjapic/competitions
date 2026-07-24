program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int32;
	b, ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := 0;
	for i := 1 to n-1 do begin
		read(b);
		inc(ans, b*i);
	end;
	readln;

	writeln(ans);
end.
