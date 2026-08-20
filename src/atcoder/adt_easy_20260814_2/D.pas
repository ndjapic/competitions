program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, d: int8;
	r, a: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, r);

	for i := 1 to n do begin
		readln(d, a);

		if (d = 1) and (1600 <= r) and (r <= 2799) or (d = 2) and (1200 <= r) and (r <= 2399) then inc(r, a);
	end;

	writeln(r);
end.
