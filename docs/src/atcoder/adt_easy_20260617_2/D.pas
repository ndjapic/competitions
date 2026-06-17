program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 50;
var
	n, i, l, r: int8;
	ans: int32;
	a: array [1 .. NN] of int32;
	s: array [0 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	s[0] := 0;
	for i := 1 to n do begin
		read(a[i]);
		s[i] := s[i-1] + a[i];
	end;
	readln;

	ans := 0;
	for l := 1 to n do
		for r := l to n do begin
			i := l;
			while (i <= r) and ((s[r] - s[l-1]) mod a[i] > 0) do inc(i);

			if i > r then inc(ans);
		end;

	writeln(ans);
end.
