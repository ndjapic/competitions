program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	b: int64;
	a: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function xpowx(x: int8): int64;
var
	i: int8;
begin
	result := 1;
	for i := 1 to x do result := result * x;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(b);

	a := 15;
	while xpowx(a) > b do dec(a);

	if xpowx(a) < b then a := -1;
	writeln(a);
end.
