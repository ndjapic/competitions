program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	prime = 998244353;
var
	n: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	if n < 0 then
		n := prime - ((-n) mod prime);

	writeln(n mod prime);
end.
