program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100 * 1000;
var
	n, i, a: int32;
	t: int64;
	s: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, t);

	s[0] := 0;
	for i := 1 to n do begin
		read(a);
		s[i] := s[i-1] + a;
	end;
	readln;

	t := t mod s[n];
	i := 1;
	while t > s[i] do inc(i);

	writeln(i, ' ', t - s[i-1]);
end.
