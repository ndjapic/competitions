program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	notc, tci, n, a, b, d, m: int32;
	ans1, ans2: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, a, b);

		d := (n-1) div 3;
		m := n - 3 * d;

		ans1 := int64(d) * min(b, 3 * a);
		ans2 := min(b, int64(m) * a);
		writeln(ans1 + ans2);

	end;
end.
