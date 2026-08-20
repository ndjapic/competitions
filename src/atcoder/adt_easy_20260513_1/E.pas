program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 50;
	KK = 2500;
	PRIME = 998244353;
var
	i, j, k, m, n, s, ans: int32;
	dp: array [0 .. NN, 0 .. KK] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure modinc(var a: int32; b: int32);
begin
	inc(a, b);
	if a >= PRIME then dec(a, PRIME);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);
	dec(m);
	dec(k, n);

	dp[0, 0] := 1;
	for s := 1 to k do dp[0, s] := 0;

	for i := 1 to n do
		for s := 0 to k do begin
			dp[i, s] := 0;
			for j := 0 to min(m, s) do modinc(dp[i, s], dp[i-1, s-j]);
		end;

	ans := 0;
	for s := 0 to k do modinc(ans, dp[n, s]);

	writeln(ans);
end.
