program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, x, y: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(x, y);

		if x mod y = 0 then
			writeln('YES')
		else
			writeln('NO');

	end;
end.
