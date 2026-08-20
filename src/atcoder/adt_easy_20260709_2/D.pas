program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
//#geometry #euclidean #distance
var
	n, i, x0, y0, x1, y1: int32;
	ans: double;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function d(x0, y0, x1, y1: int64): double;
begin
	result := sqrt(sqr(x1-x0) + sqr(y1-y0));
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := 0.0;
	x0 := 0;
	y0 := 0;
	for i := 1 to n do begin
		readln(x1, y1);
		ans := ans + d(x0, y0, x1, y1);
		x0 := x1;
		y0 := y1;
	end;
	ans := ans + d(x0, y0, 0, 0);

	writeln(ans:0:6);
end.
