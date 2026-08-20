program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	notc, tci, n, i, a, mn, mx: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		mn := 1000;
		mx := 1;
		for i := 1 to n do begin
			read(a);
			mn := min(mn, a);
			mx := max(mx, a);
		end;
		readln;

		writeln((mx - mn + 1) div 2);

	end;
end.
