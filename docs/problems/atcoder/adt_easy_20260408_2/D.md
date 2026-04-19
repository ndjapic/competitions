# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x: int8;
	ans: int16;
	ch: char;
	s: string;
	a: array ['A' .. 'Z'] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	for x := 1 to 26 do a[s[x]] := x;

	x := a['A'];
	ans := 0;
	for ch := 'B' to 'Z' do begin
		inc(ans, abs(a[ch] - x));
		x := a[ch];
	end;

	writeln(ans);
end.

```
