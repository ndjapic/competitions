program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 5;
var
	notc, tci, n, i, h, l, r: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		l := 6;
		r := 1;

		for i := 1 to n do begin
			read(h);
			l := min(l, h);
			r := max(r, h);
		end;
		readln;

		writeln(r-l+1);

	end;
end.
