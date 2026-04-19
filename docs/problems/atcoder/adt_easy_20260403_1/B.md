# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, l, r: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, l, r);
	readln(s);

	i := l;
	while (i <= r) and (s[i] = 'o') do inc(i);

	if i > r then
		writeln('Yes')
	else
		writeln('No');
end.

```
