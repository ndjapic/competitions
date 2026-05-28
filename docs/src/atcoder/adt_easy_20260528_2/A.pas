program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, t0, t1, v: int8;
	ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := 0;
	t0 := 0;
	for i := 1 to n do begin
		readln(t1, v);
		ans := max(0, ans - t1 + t0);
		inc(ans, v);
		t0 := t1;
	end;

	writeln(ans);
end.
