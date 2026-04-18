# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, i: int8;
	s: array [1 .. nn] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do readln(s[i]);

	i := 2;
	while (i < n) and not ((s[i] = 'sweet') and (s[i+1] = 'sweet')) do inc(i);

	if i >= n-1 then
		writeln('Yes')
	else
		writeln('No');
end.

```
