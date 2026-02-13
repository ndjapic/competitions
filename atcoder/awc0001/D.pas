program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections;
const
	nn = 200;
var
	i, j, k, m, n, l, r: int32;
	a, b: array [1 .. nn] of int32;
	dp: array [0 .. nn, 0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	for j := 0 to m do dp[0, j] := 0;

	for r := 1 to n do begin
		readln(a[r], b[r]);
		for j := b[r] to m do begin
			mx := -n*m;
			for l := max(0, r-k) to r-1 do
				mx := max(mx, dp[l, j-b[r]]);
			dp[r, j] := mx + a[r] - b[r];
		end;
	end;

end.
