# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100 * 1000;
var
	n, t, i, i0, j, mx, ans: int32;
	s: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, t);

	ans := 0;
	for j := 1 to t do begin

		mx := 0;
		i0 := 1;
		for i := 1 to n do begin
			read(s[i]);
			if s[i] > s[i0] then i0 := i;
		end;
		readln;
		mx := s[i0] div 2;

		i := 1;
		while (i <= n) and ((i = i0) or (s[i] <= mx)) do inc(i);
		if i > n then inc(ans);

	end;

	writeln(ans);
end.

```
