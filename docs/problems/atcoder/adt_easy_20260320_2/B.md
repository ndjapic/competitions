# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	h, w, i, j, ans: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	ans := 0;
	for i := 1 to h do begin
		readln(s);
		for j := 1 to w do
			if s[j] = '#' then inc(ans);
	end;

	writeln(ans);
end.

```
