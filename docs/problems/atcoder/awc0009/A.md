# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	k, i, m, l, s: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(k, m);

	s := 0;
	for i := 1 to k do begin
		read(l);
		inc(s, l);
	end;
	readln;

	writeln(s mod m);
end.

```
