# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, i: int8;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);
	n := length(s);
	m := length(t);

	i := 1;
	while (i <= n) and (i <= m) and (s[i] = t[i]) do inc(i);
	if (i > n) and (i > m) then i := 0;
	writeln(i);
end.

```
