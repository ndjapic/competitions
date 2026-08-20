program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x, y, a, b, s: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x, y);

	s := 0;
	for a := 1 to 6 do
		for b := 1 to 6 do
			if (a+b >= x) or (abs(a-b) >= y) then inc(s);

	writeln(s/36 :0:9);
end.
