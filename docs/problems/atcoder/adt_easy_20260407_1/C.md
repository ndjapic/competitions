# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 50;
var
	n, k, l, r, ans: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	readln(s);

	l := 0;
	ans := 0;
	for r := 1 to n do
		if s[r] = 'X' then
			l := r
		else if r-l = k then begin
			inc(ans);
			l := r;
		end;

	writeln(ans);
end.

```
