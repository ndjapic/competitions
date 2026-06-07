program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	HH = 500;
var
	h, w, i, j, k, x, l, r: int32;
	ans: int64;
	s: string;
	dp: array [0 .. HH, 0 .. HH] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function rec(i1, j1, i2, j2: int32): int32;
begin
	result := dp[i2, j2] - dp[i2, j1] + dp[i1, j1] - dp[i1, j2];
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w, k);

	for j := 0 to w do dp[0, j] := 0;

	ans := 0;
	for i := 1 to h do begin
		dp[i, 0] := 0;
		readln(s);
		for j := 1 to w do begin
			dp[i, j] := dp[i, j-1] - dp[i-1, j-1] + dp[i-1, j];
			if s[j] = '1' then inc(dp[i, j]);

			l := j-1;
			r := j-1;
			for x := 0 to i-1 do begin
				while (l >= 0) and (rec(x, l, i, j) <= k) do dec(l);
				while (r >= 0) and (rec(x, r, i, j) < k) do dec(r);
				inc(ans, r-l);
			end;
		end;
	end;

	writeln(ans);
end.
