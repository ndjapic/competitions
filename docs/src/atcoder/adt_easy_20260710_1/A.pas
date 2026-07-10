program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	h: int32;
	e: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h);
	inc(h);

	e := 0;
	while h shr e > 0 do inc(e);

	writeln(e);
end.
