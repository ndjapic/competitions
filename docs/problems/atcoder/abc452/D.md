# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
	mm = 50;
var
	n, i: int32;
	m, j: int8;
	ans: int64;
	s, t: string;
	dp: array [0 .. mm] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);
	n := length(s);
	m := length(t);

	for j := 0 to m do dp[j] := 0;

	ans := 0;
	for i := 1 to n do begin
		for j := m downto 1 do begin
			if (s[i] = t[j]) and (dp[j-1] > 0) then dp[j] := i;
			inc(ans, i - dp[m]);
		end;
	end;

	writeln(ans);
end.

```
