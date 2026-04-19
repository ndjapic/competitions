# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, l, r: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(l, r);
	readln(s);
	n := length(s);

	for i := 1 to l-1 do write(s[i]);
	for i := r downto l do write(s[i]);
	for i := r+1 to n do write(s[i]);
	writeln;
end.

```
