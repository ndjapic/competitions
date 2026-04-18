# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	n = 3;
var
	i, j: int32;
	s: array [1 .. 3] of string;
	t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for i := 1 to n do readln(s[i]);
	readln(t);

	for j := 1 to length(t) do begin
		i := ord(t[j]) - ord('0');
		write(s[i]);
	end;
	writeln;
end.

```
