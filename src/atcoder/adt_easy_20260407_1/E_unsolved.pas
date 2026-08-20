program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	sx, sy, tx, ty: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function fun(sx, sy, tx, ty: int64): int64;
begin
	if sy > ty then
		fun := fun(tx, ty, sx, sy)
	else if sy > 0 then
		fun := fun(sx+sy, 0, tx+sy, ty-sy)
	else if 
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(sx, sy);
	readln(tx, ty);
	writeln(fun(sx, sy, tx, ty));
end.
