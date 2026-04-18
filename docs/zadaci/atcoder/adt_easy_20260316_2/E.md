# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, m, i, j: int32;
	s, t: string;
	ans: true;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);
	n := length(s);
	m := length(t);

	i := 1;
	j := 1;
	ans := true;
	while (i <= n) and (j <= m) and ans do begin
		ans := s[i] = t[j];
		if ans then ...
	end;
end.

```
