program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b, k: int64;
	e: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b, k);

	e := 0;
	while a < b do begin
		a := a * k;
		inc(e);
	end;

	writeln(e);
end.
