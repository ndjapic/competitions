# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, k, i, l, r, ans: int32;
	s: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do read(s[i]);
	readln;

	ans := 0;
	l := 0;
	for r := 1 to n do
		if s[r] = 0 then
			l := r
		else if ((r = n) or (s[r+1] = 0)) and (r-l >= k) then
			inc(ans);

	writeln(ans);
end.

```
