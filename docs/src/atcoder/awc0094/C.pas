program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, x, l, r: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	l := 1 shl 30;
	r := 0;
	for i := 1 to n do begin
		read(x);
		l := min(l, x);
		r := max(r, x);
	end;
	readln;

	writeln((r-l) * 2);
end.
