# Задатак: A.pas

```pascal
program _A;
{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int32;
	a, b, k: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	ans := 0;
	for i := 1 to n do begin
		readln(a, b);
		if a*b >= k then inc(ans);
	end;

	writeln(ans);
end.

```
