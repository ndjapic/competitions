program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, x, y: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(x);

		y := 10;
		while y <= x do y := y * 10;

		writeln(y+1);

	end;
end.
