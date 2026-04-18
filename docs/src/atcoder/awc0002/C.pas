program _C;
{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	n, m, i, a, b, d: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	d := 0;
	for i := 1 to n do begin
		read(a, b);
		if a < m then d := max(d, (m-a+b-1) div b);
	end;
	readln;

	writeln(d);
end.
