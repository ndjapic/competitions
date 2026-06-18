program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 100 * 1000;
var
	n, k, i, mn, mx: int32;
	a: int8;
	s: array [0 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	s[0] := 0;
	for i := 1 to n do begin
		read(a);
		s[i] := s[i-1] + a;
	end;
	readln;

	mn := 1 shl 30;
	mx := 0;

	for i := k to n do begin
		mn := min(mn, s[i] - s[i-k]);
		mx := max(mx, s[i] - s[i-k]);
	end;

	writeln(mx - mn);
end.
