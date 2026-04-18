# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	n = 7;
var
	notc, tci, i, s, ans: int32;
	a: array [1 .. n] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		s := 0;
		for i := 1 to n do begin
			read(a[i]);
			a[i] := -a[i];
			inc(s, a[i]);
		end;

		ans := low(int32);
		for i := 1 to n do
			ans := max(ans, s-2*a[i]);

		writeln(ans);

	end;
end.

```
