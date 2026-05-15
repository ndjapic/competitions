program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function pow(a: int32; b: int8): int32;
begin
	if b = 0 then
		result := 1
	else if odd(b) then
		result := pow(a, b-1) * a
	else
		result := pow(sqr(a), b div 2);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b);
	writeln(pow(a, b));
end.
