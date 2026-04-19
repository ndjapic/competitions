# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	i := 1;
	while (i <= n) and (s[i] = 'A') do inc(i);
	while (i <= n) and (s[i] = 'B') do inc(i);
	while (i <= n) and (s[i] = 'C') do inc(i);

	if i > n then
		writeln('Yes')
	else
		writeln('No');
end.

```
