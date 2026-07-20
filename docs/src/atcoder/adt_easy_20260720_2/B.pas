program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	ans := 0;
	while m > 0 do begin
		m := n mod m;
		inc(ans);
	end;

	writeln(ans);
end.
