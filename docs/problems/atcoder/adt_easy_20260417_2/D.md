# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	hh = 20;
var
	h, w, i, j, c: int8;
	s: array [1 .. hh] of string;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	for i := 1 to h do readln(s[i]);

	ans := true;
	for i := 1 to h do
		for j := 1 to w do
			if s[i][j] = '#' then begin
				c := 0;
				if (i > 1) and (s[i-1][j] = '#') then inc(c);
				if (j > 1) and (s[i][j-1] = '#') then inc(c);
				if (i < h) and (s[i+1][j] = '#') then inc(c);
				if (j < w) and (s[i][j+1] = '#') then inc(c);
				ans := ans and ((c = 2) or (c = 4));
			end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.

```
