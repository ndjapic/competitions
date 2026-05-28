program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, x, i, s, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x);

	ans := 0;
	for i := 1 to n do begin
		read(s);
		if s <= x then inc(ans, s);
	end;
	readln;

	writeln(ans);
end.
