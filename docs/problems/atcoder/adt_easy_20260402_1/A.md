# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, d, i, empty: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d);
	readln(s);

	empty := 0;
	for i := 1 to n do
		if s[i] = '.' then
			inc(empty)
		else if d > 0 then begin
			inc(empty);
			dec(d);
		end;

	writeln(empty);
end.

```
