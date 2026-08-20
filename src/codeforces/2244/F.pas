program _F;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unsolved
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

		for i := 1 to n do begin
			read(a);
		end;
		readln;

		writeln((mx - mn + 1) div 2);

	end;
end.
