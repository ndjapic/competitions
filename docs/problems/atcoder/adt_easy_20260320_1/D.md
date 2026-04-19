# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int8;
	x: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x);

	n := length(x);
	while x[n] = '0' do dec(n);
	if x[n] = '.' then dec(n);

	setlength(x, n);
	writeln(x);
end.

```
