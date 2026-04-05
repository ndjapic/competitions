program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 5;
var
	n, a, b, mx, a2, b2, aa, ab, ba, bb, ans: int64;
	i: int32;
	dpa, dpb: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, a, b);
	a2 := a div 2;
	b2 := b div 2;

	if n > nn then begin

		mx := max(a, b);

		aa := a + a2 * (n-2) + mx div 2;

		bb := b * (n-1) + mx;

		ab := (n-1) div 2 * (a + b2);
		if odd(n-1) then
			inc(ab, max(a + mx div 2, b + mx))
		else
			inc(ab, mx);

		ba := b + (n-2) div 2 * (a + b2);
		if odd(n-2) then
			inc(ba, max(a + mx div 2, b + mx))
		else
			inc(ba, mx);

		ans := max(max(aa, ab), max(ba, bb));
		writeln(ans);

	end else begin

		dpa[0] := 0;
		dpb[0] := 0;

		for i := 1 to n do begin
			dpa[i] := max(dpa[i-1] + a2, dpb[i-1] + a);
			dpb[i] := max(dpa[i-1] + b2, dpb[i-1] + b);
		end;

		writeln(max(dpa[n], dpb[n]));

	end;
end.
