program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 500 * 1000;
	INF = int64(1) shl 60;
var
	n, k, i: int32;
	ans: int64;
	a: array [1 .. NN] of int32;
	s, s0: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	s[0] := 0;
	s0[0] := 0;
	for i := 1 to n do begin
		read(a[i]);
		s[i] := s[i-1] + a[i];
		s0[i] := s0[i-1] + max(0, a[i]);
	end;
	readln;

	ans := -INF;
	for i := k to n do
		ans := max(ans, s0[i-k] + s[i] - s[i-k] + s0[n] - s0[i]);

	writeln(ans);
end.
