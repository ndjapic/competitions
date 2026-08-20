program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #classic #kadane #algorithm
uses
	math;
const
	nn = 200 * 1000;
	inf = int64(1) shl 60;
var
	n, i, b: int32;
	s, mx: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	s := 0;
	mx := -inf;

	for i := 1 to n do begin
		read(b);
		s := max(0, s) + b;
		mx := max(mx, s);
		writeln(mx);
	end;
	readln;
end.
