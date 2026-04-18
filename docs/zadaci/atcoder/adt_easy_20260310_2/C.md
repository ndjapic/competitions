# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, m, i, j, d: int8;
	s, t: string;
	c, mn: int16;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	readln(s);
	readln(t);

	mn := 1000;
	for i := 0 to n-m do begin
		c := 0;
		for j := 1 to m do begin
			d := ord(s[i+j]) - ord(t[j]);
			if d < 0 then inc(d, 10);
			inc(c, d);
		end;
		mn := min(mn, c);
	end;

	writeln(mn);
end.

```
