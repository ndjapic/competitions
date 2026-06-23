program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #prefix #sum
uses
	math;
const
	NN = 200 * 1000;
var
	n, k, i, t: int32;
	m, ans: int64;
	s: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	s[0] := 0;
	for i := 1 to k-1 do begin
		read(t);
		s[i] := s[i-1] + 1000 * t;
	end;

	ans := 0;
	for i := k to n do begin
		read(t);
		s[i] := s[i-1] + 1000 * t;
		m := (s[i] - s[i-k]) div k;
		ans := max(ans, m);
	end;
	readln;

	writeln(ans);
end.
