program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci: int32;
	px, py, qx, qy, rx, ry, sx, sy: int64;
	mx1, my1, mx2, my2, dx1, dy1, dx2, dy2, dx, dy: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(px, py, qx, qy, rx, ry, sx, sy);

		mx1 := qx + px;
		my1 := qy + py;
		mx2 := sx + rx;
		my2 := sy + ry;

		dx1 := py - qy;
		dy1 := qx - px;
		dx2 := ry - sy;
		dy2 := sx - rx;

		dx := mx2 - mx1;
		dy := my2 - my1;

		// dy1 / dx1 = dy2 / dx2

		if dy1 * dx2 <> dy2 * dx1 then
			writeln('Yes')
		else if dy1 * dx = dy * dx1 then
			writeln('Yes')
		else
			writeln('No');

	end;
end.
