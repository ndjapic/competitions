# Задатак: A.pas

```pascal
program _A;
{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci: int32;
	n, m, d, k: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, m, d);

		k := d div m + 1;

		writeln((n + k - 1) div k);

	end;
end.

```
