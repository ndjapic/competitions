program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, nice: int32;
	l, r, t: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, l, r);

	nice := 0;
	for i := 1 to n do begin
		read(t);
		if (l <= t) and (t <= r) then inc(nice);
	end;
	readln;

	writeln(nice);
end.
