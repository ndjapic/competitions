program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp #mod
const
	NN = 100 * 1000;
	prime = 1000 * 1000 * 1000 + 7;
var
	n, m, i, j: int32;
	dp: array [0 .. NN] of int64;
	broken: array [0 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 0 to n do broken[i] := false;

	for j := 1 to m do begin
		read(i);
		broken[i] := true;
	end;
	if m > 0 then readln;

	dp[0] := 1;
	for i := 1 to n do begin

		if broken[i-1] then
			dp[i] := 0
		else
			dp[i] := dp[i-1];

		if (i >= 2) and not broken[i-2] then inc(dp[i], dp[i-2]);
		if (i >= 3) and not broken[i-3] then inc(dp[i], dp[i-3]);
		while dp[i] >= prime do dec(dp[i], prime);
		// dp[i] := dp[i] mod prime;

	end;

	writeln(dp[n]);
end.
