# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);
	ans := 0;

	for i := 1 to n do
		if s[i] = 'w' then
			inc(ans, 2)
		else
			inc(ans);

	writeln(ans);
end.

```
