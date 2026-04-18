# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, p, b, k, c, pay: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, p, b, k);

	pay := 0;
	for i := 1 to n do begin
		read(c);
		if c >= k then
			inc(pay, (p+b) * c)
		else
			inc(pay, p * c);
	end;
	readln;

	writeln(pay);
end.

```
