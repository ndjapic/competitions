program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x, p10, y, z: int64;
	k, i: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(x, k);

	p10 := 1;
	for i := 1 to k do begin
		p10 := p10 * 10;
		y := x div p10 * p10;
		z := y + p10;

		if x - y < z - x then
			x := y
		else
			x := z;
	end;

	writeln(x);
end.
