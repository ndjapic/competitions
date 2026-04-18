# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	s: string;
	found: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	for i := 1 to n do
		if (s[i] <> 'a') and
			(s[i] <> 'e') and
			(s[i] <> 'i') and
			(s[i] <> 'o') and
			(s[i] <> 'u')
		then
			write(s[i]);
end.

```
