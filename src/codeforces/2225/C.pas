program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, d: int32;
	s1, s2: string;
	dp: array [0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s1);
		readln(s2);

		dp[0] := 0;
		for i := 1 to n do begin
			dp[i] := dp[i-1];
			if s1[i] <> s2[i] then inc(dp[i]);
			if i > 1 then begin
				d := dp[i-2];
				if s1[i-1] <> s1[i] then inc(d);
				if s2[i-1] <> s2[i] then inc(d);
				dp[i] := min(dp[i], d);
			end;
		end;

		writeln(dp[n]);

	end;
end.
