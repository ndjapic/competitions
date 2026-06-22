program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i: int32;
	s, k, e, mn: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, s, k);

	mn := 0;
	for i := 1 to n-1 do begin
		read(e);
		mn := min(mn, s - e);
		inc(s, e);
	end;
	readln;

	mn := -mn;
	if mn > k then mn := -1;
	writeln(mn);
end.
