program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, k, a: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k, a);

	a := (a+k-2) mod n + 1;

	writeln(a);
end.
