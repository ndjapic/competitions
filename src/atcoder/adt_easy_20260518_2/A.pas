program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, x: int32;
	ch: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x);

	x := (x-1) div n;
	ch := chr(ord(x) + ord('A'));

	writeln(ch);
end.
