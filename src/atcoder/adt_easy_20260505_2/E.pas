program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp #modulo
const
	nn = 1000 * 1000;
	prime = 998244353;
var
	n, i, ans: int32;
	d: int8;
	dp: array [1 .. nn, 1 .. 9] of uint32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for d := 1 to 9 do dp[1, d] := 1;

	for i := 2 to n do begin
		for d := 1 to 9 do dp[i, d] := dp[i-1, d];
		for d := 1 to 8 do begin
			inc(dp[i, d], dp[i-1, d+1]);
			inc(dp[i, d+1], dp[i-1, d]);
		end;
		for d := 1 to 9 do
			while dp[i, d] >= prime do dec(dp[i, d], prime);
	end;

	ans := 0;
	for d := 1 to 9 do begin
		inc(ans, dp[n, d]);
		if ans >= prime then dec(ans, prime);
	end;

	writeln(ans);
end.
