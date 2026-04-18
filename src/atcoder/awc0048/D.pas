program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200;
var
	n, m, i, j, k, h, w, ans: int32;
	s: string;
	dp: array [0 .. nn, 0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	for j := 0 to m do dp[0, j] := 0;

	for i := 1 to n do begin
		readln(s);
		dp[i, 0] := 0;
		for j := 1 to m do
			dp[i, j] := dp[i, j-1] - dp[i-1, j-1] + dp[i-1, j] + ord(s[j]) - ord('0');
	end;

	h := 1;
	w := m;
	ans := -1;
	while (h <= n) and (w > 0) do
		if h * w > k then
			dec(w)
		else begin
			if h * w = k then
				for i := h to n do
					for j := w to m do
						ans := max(ans, dp[i, j] - dp[i, j-w] + dp[i-h, j-w] - dp[i-h, j]);
			inc(h);
		end;

	writeln(ans);
end.
