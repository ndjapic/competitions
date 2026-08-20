program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 50;
var
	n, i, l, r: int8;
	ans, dif: int32;
	a: array [1 .. NN] of int32;
	s: array [0 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	s[0] := 0;
	ans := 0;
	for r := 1 to n do begin
		read(a[r]);
		s[r] := s[r-1] + a[r];

		for l := 1 to r do begin

			i := l;
			dif := s[r] - s[l-1];
			while (i <= r) and (dif mod a[i] > 0) do inc(i);

			if i > r then inc(ans);

		end;
	end;
	readln;

	writeln(ans);
end.
