program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := 0;
	while n > 0 do begin
		inc(ans, n mod 10);
		n := n div 10;
	end;

	writeln(111 * ans);
end.
