program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, x, s, total: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x);

	total := 0;
	for i := 1 to n do begin
		read(s);
		if s <= x then inc(total, s);
	end;
	readln;

	writeln(total);
end.
