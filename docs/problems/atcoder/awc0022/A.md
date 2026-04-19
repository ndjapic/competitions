# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, d, ans: int32;
	k, w: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d, k);
	k := k * d;

	ans := 0;
	for i := 1 to n do begin
		read(w);
		if w > k then inc(ans);
	end;
	readln;

	writeln(ans);
end.

```
