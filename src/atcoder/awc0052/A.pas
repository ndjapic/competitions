program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	n, l, r, ans: int32;
	a: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	read(a[1]);
	ans := 1;
	l := 1;

	for r := 2 to n do begin
		read(a[r]);
		if a[r-1] >= a[r] then
			l := r
		else
			ans := max(ans, r-l+1);
	end;
	readln;

	writeln(ans);
end.
