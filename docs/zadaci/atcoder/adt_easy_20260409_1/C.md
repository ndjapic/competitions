# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, p10: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	p10 := 1;
	while n >= 1000 do begin
		n := n div 10;
		p10 := p10 * 10;
	end;

	writeln(n * p10);
end.

```
