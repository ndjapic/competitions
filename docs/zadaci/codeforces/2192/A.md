# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	notc, tci, n, i, ans: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		ans := 1;
		for i := 2 to n do
			if s[i-1] <> s[i] then inc(ans);

		if (ans < n) and (s[1] <> s[n]) then inc(ans);
		writeln(ans);

	end;
end.

```
