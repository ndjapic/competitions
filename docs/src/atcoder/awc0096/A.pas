program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i: int32;
	a1, a2, b: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	read(a1, b);
	for i := 2 to n do begin
		inc(a1, b);
		readln(a2, b);
		a1 := max(a1, a2);
	end;

	writeln(a1 + b);
end.
