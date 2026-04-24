program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function f(x: int32): int32;
begin
	result := sqr(x) + 2 * x + 3;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	t := 'aeiou';
	for i := 1 to n do
		if pos(s[i], t) = 0 then
			write(s[i]);
	writeln;
end.
