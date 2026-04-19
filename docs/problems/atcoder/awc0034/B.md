# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i, ans: int32;
	p: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n-1 do read(p[i]); readln;

	ans := 1;
	i := 1;
	while i < n do begin
		i := p[i];
		inc(ans);
	end;

	writeln(ans);
end.

```
