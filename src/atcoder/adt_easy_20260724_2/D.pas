program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x: int32;
	d: int8;
	c: array [0 .. 9] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x);

	for d := 0 to 9 do c[d] := 0;

	while x > 0 do begin
		inc(c[x mod 10]);
		x := x div 10;
	end;

	x := 1;
	while c[x] = 0 do inc(x);
	dec(c[x]);

	for d := 0 to 9 do
		while c[d] > 0 do begin
			x := 10 * x + d;
			dec(c[d]);
		end;

	writeln(x);
end.
