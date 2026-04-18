# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	hh = 500;
var
	h, w, n, i, j, k, ans: int32;
	s: array [1 .. hh] of string;
	t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w, n);

	for i := 1 to h do readln(s[i]);

	readln(t);

	i := 1;
	j := 1;
	ans := n;

	if s[i][j] = '#' then s[i][j] := '.';

	for k := 1 to n do begin

		case t[k] of
			'U': i := max(1, i-1);
			'D': i := min(h, i+1);
			'L': j := max(1, j-1);
			'R': j := min(w, j+1);
		end;

		if s[i][j] = '#' then s[i][j] := '.';

	end;

	ans := 0;
	for i := 1 to h do
		for j := 1 to w do
			if s[i][j] = '#' then inc(ans);
	writeln(ans);
end.

```
