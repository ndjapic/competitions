program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i, ans: int32;
	k: int8;
	a: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	ans := 0;
	for i := 1 to n do begin
		read(a[i]);
		if (a[i] >= k) and ((ans = 0) or (a[i-1] < k)) then inc(ans);
	end;
	readln;

	writeln(ans);
end.
