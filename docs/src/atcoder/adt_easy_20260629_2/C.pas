program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x, p10: int64;
	k, i: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x, k);

	p10 := 1;
	for i := 1 to k do begin
		inc(x, 5 * p10);
		p10 := p10 * 10;
		dec(x, x mod p10);
	end;

	writeln(x);
end.
