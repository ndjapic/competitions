program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int64;
	k: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	k := 59;
	while int64(1) shl k > n do dec(k);

	writeln(k);
end.
