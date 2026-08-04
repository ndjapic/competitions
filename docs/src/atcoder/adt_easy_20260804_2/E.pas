program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, l, r: int32;
	ans: int64;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	l := 1;
	ans := 0;
	for r := 1 to n do begin
		read(a[r]);
		if (r-l+1 >= 3) and (a[r] - a[r-1] <> a[r-1] - a[r-2]) then l := r-1;
		inc(ans, r-l+1);
	end;
	readln;

	writeln(ans);
end.
