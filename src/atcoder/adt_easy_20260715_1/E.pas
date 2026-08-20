program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #fixpoint
const
	NN = 500 * 1000;
var
	n, i: int32;
	a, dp: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function fixpoint(s: int32): int32;
begin
	if dp[s] = 0 then begin
		if a[s] = s then
			dp[s] := s
		else
			dp[s] := fixpoint(a[s]);
	end;
	result := dp[s];
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do begin
		read(a[i]);
		dp[i] := 0;
	end;
	readln;

	for i := 1 to n do begin
		write(fixpoint(i));
		if i < n then
			write(' ');
	end;
end.
