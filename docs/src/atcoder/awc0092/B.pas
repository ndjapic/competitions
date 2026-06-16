program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, l, t, i: int32;
	x, v: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, l, t);

	for i := 1 to n do begin
		readln(x, v);
		inc(x, v * t);
		x := abs(x);
		x := x mod (2*l);
		if x > l then x := 2*l - x;
		writeln(x);
	end;
end.
