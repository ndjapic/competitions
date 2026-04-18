# Задатак: B_Looped_Rope.pas

```pascal
program B_Looped_Rope;
{$MODE DELPHI}
const
	hh = 20;
var
	h, w, i, j, black: int8;
	ans: boolean;
	s: array [1 .. hh] of string;

begin
	readln(h, w);

	for i := 1 to h do readln(s[i]);

	ans := true;
	for i := 1 to h do
		for j := 1 to w do
			if s[i][j] = '#' then begin
				black := 0;
				if (i > 1) and (s[i-1][j] = '#') then inc(black);
				if (j > 1) and (s[i][j-1] = '#') then inc(black);
				if (i < h) and (s[i+1][j] = '#') then inc(black);
				if (j < w) and (s[i][j+1] = '#') then inc(black);
				ans := ans and ((black = 2) or (black = 4));
			end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.

```
