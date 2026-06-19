program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, x, y, z: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x, y, z);

	if ((x <= z) and (z <= y)) or ((x >= z) and (z >= y)) then
		write('Yes')
	else
		write('No');
end.
