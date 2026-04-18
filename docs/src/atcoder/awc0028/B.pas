program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	n, l, r, ll, rr, harvestable: int32;
	t: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, ll, rr);

	harvestable := 0;
	l := 1;
	for r := 1 to n do begin
		read(t[r]);
		if (ll <= t[r]) and (t[r] <= rr) then
			harvestable := max(harvestable, r-l+1)
		else
			l := r+1;
	end;
	readln;

	writeln(harvestable);
end.
