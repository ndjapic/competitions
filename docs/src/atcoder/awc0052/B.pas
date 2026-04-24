program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int32;
	k: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	k := k mod n;

	for i := n-k+1 to n do writeln(i);
	for i := 1 to n-k do writeln(i);
end.
