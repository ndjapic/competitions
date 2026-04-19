# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	h, w, i, j, k, x: int32;
	c1, c2: char;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w, k);
	setlength(t, w*k);
	readln(c1, c2, c2);

	for i := 1 to h do begin
		readln(s);

		for j := 1 to w do
			if s[j] = '#' then
				for x := 1 + j*k - k to j*k do t[x] := c1
			else
				for x := 1 + j*k - k to j*k do t[x] := c2;

		for x := 1 to k do writeln(t);
	end;
end.

```
