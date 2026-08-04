program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	notc, tci, a, b, c, ab, bc, ca, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(a, b, c);

		ab := abs(a - b);
		bc := abs(b - c);
		ca := abs(c - a);

		ans := min(min(ab, bc), ca);

		writeln(ans);

	end;
end.
