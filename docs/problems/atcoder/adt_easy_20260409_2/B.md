# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, s, k, i, p, q: int32;
	ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, s, k);
	ans := 0;

	for i := 1 to n do begin
		readln(p, q);
		inc(ans, p*q);
	end;

	if ans < s then inc(ans, k);
	writeln(ans);
end.

```
