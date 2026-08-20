program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 200 * 1000;
var
	n, m, i, a: int32;
	ans: int64;
	s0, s1: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	s0[0] := 0;
	s1[0] := 0;
	for i := 1 to n do begin
		read(a);
		s0[i] := s0[i-1] + a;
		s1[i] := s1[i-1] + int64(i) * a;
	end;
	readln;

	ans := low(int64);

	for i := m to n do
		ans := max(ans, s1[i] - s1[i-m] - (s0[i] - s0[i-m]) * (i-m));

	writeln(ans);
end.
