program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	notc, tci: int8;
	a, b, c, mx, mn, ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(a, b, c);

		mx := max(a, b);
		mx := max(mx, c);
		mn := min(a, b);
		mn := min(mn, c);

		ans := mx - mn;

		ans := min(ans, a+b - min(a, b));
		ans := min(ans, b+c - min(b, c));
		ans := min(ans, c+a - min(c, a));

		writeln(ans);

	end;
end.
