# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, a: int32;
	s, m: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	s := 0;
	for i := 1 to n do begin
		read(a);
		inc(s, a);
	end;
	readln;

	if s < m then
		writeln('No')
	else
		writeln('Yes');
end.

```
