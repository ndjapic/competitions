# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000 + 2;
var
	n, i, ans: int32;
	s: array [-1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	s[-1] := 0;
	s[0] := 0;
	s[n+1] := 0;
	s[n+2] := 0;

	ans := 0;
	for i := 1 to n do read(s[i]);
	readln;

	ans := 0;
	for i := 1 to n do
		if (s[i] = 0) and
			(s[i-2] + s[i-1] < 2) and
			(s[i-1] + s[i+1] < 2) and
			(s[i+1] + s[i+2] < 2)
		then begin
			s[i] := 1;
			inc(ans);
		end;

	writeln(ans);
end.

```
