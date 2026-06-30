program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, k: int8;
	a, p10, x: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	p10 := 1;
	for i := 1 to k do p10 := p10 * 10;

	x := 1;
	for i := 1 to n do begin
		read(a);
		if x >= (p10 + a-1) div a then
			x := 1
		else
			x := x * a;
	end;
	readln;

	writeln(x);
end.
