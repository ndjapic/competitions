# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, j, k, a, b, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	ans := 0;
	for i := 1 to n do begin
		read(a);
		for j := 1 to a do begin
			read(b);
			if b >= k then
				inc(ans);
		end;
		readln;
	end;

	writeln(ans);
end.

```
