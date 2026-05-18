program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i: int8;
	a, t0, t1: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, a);

	t0 := 0;
	for i := 1 to n do begin
		read(t1);
		t1 := max(t1, t0);
		inc(t1, a);
		writeln(t1);
		t0 := t1;
	end;
	readln;
end.
