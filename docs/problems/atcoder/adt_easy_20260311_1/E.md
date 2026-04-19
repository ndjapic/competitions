# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	k: int64;
	e, i: int8;
	s: string;
	a: array of char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(k);
	setlength(a, 1);
	e := 0;

	while k > 0 do begin
		if length(a) = e then setlength(a, 2*e);
		a[e] := chr(ord('0') + k mod 2 * 2);
		k := k div 2;
		inc(e);
	end;

	setlength(s, e);
	for i := 1 to e do s[i] := a[e-i];
	writeln(s);
end.

```
