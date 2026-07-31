program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, m, d, k, ans: int32;
	a, limit: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	Readln(n, m, d, k);
	k := min(k, m);

	limit := int64(m) * d;
	ans := n;

	for i := 1 to n do begin
		read(a);
		if a <= limit then dec(ans);
	end;
	readln;

	writeln(max(0, ans - k));
end.
