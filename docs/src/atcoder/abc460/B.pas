program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci: int8;
	x1, y1, r1, x2, y2, r2, d2: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(x1, y1, r1, x2, y2, r2);

		d2 := sqr(x1-x2) + sqr(y1-y2);
		if (sqr(r1-r2) <= d2) and (d2 <= sqr(r1+r2)) then
			writeln('Yes')
		else
			writeln('No');

	end;
end.
