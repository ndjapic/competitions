program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100 * 1000;
	prime = 1000 * 1000 * 1000 + 7;
var
	n, i: int32;
	s: string;
	dp: array [-2 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	dp[-2] := 0;
	dp[-1] := 0;
	dp[0] := 1;

	for i := 1 to n do begin
		dp[i] := 0;
		if s[i] = '.' then begin
			inc(dp[i], dp[i-3]);
			inc(dp[i], dp[i-2]);
			inc(dp[i], dp[i-1]);
			dp[i] := dp[i] mod prime;
		end;
	end;

	writeln(dp[n]);
end.
