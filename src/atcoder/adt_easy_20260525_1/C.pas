program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #right #trinagle #euclid #distance
var
	xa, ya, xb, yb, xc, yc, a2, b2, c2: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function d2(x1, y1, x2, y2: int32): int32;
begin
	result := sqr(x1 - x2) + sqr(y1 - y2);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(xa, ya);
	readln(xb, yb);
	readln(xc, yc);

	a2 := d2(xb, yb, xc, yc);
	b2 := d2(xc, yc, xa, ya);
	c2 := d2(xa, ya, xb, yb);

	if a2 + b2 = c2 then
		writeln('Yes')
	else if b2 + c2 = a2 then
		writeln('Yes')
	else if c2 + a2 = b2 then
		writeln('Yes')
	else
		writeln('No');
end.
