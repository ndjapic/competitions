program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int64;
	l, r, m: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	l := 0;
	r := 60;
	while r-l > 1 do begin
		m := (l+r) div 2;

		if n shr m > 0 then
			l := m
		else
			r := m;
	end;

	writeln(l);
end.
