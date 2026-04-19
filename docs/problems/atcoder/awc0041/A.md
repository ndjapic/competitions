# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100 * 1000;
var
	n, m, i, j: int32;
	ans: int64;
	c, k: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do readln(c[i], k[i]);

	ans := 0;
	for j := 1 to m do begin
		readln(i);
		if k[i] > 0 then begin
			inc(ans, c[i]);
			dec(k[i]);
		end;
	end;
	writeln(ans);
end.

```
