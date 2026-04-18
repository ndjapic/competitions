program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, c, s, mn: int32;
	m: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do begin
		readln(c, s);
		mn := min(m, c-s);
		dec(m, mn);
		inc(s, mn);
		writeln(s);
	end;
end.
