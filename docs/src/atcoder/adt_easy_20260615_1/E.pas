program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
	XX = 10 * 1000;
var
	n, i, j, x, a, b, l, r: int32;
	dp: array [0 .. NN, 0 .. XX] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x);

	dp[0, 0] := true;
	l := 0;
	r := 0;

	for i := 1 to n do begin
		readln(a, b);
		inc(l, a);
		inc(r, b);

		for j := l to r do
			dp[i, j] := false;

		for j := l-a to r-b do
			if dp[i-1, j] then begin
				dp[i, j+a] := true;
				dp[i, j+b] := true;
			end;
	end;

	if (l <= x) and (x <= r) and dp[n, x] then
		writeln('Yes')
	else
		writeln('No');
end.
