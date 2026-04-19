# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	h, w, r, c, ans: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);
	readln(r, c);

	ans := 0;
	if r > 1 then inc(ans);
	if c > 1 then inc(ans);
	if r < h then inc(ans);
	if c < w then inc(ans);
	writeln(ans);
end.

```
