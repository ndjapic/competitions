program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	prime = 1000 * 1000 * 1000 + 7;
var
	n, i: int32;
	a, b, c, bento, ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, a, b, c);

	bento := a * b * c mod prime;
	if bento < n then
		ans := 0
	else begin
		ans := 1;
		for i := bento - n + 1 to bento do
			ans := ans * i mod prime;
	end;

	writeln(ans);
end.
