program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	a, b, c, d, e, f, x, tak, aok: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function dist(a, b, c, x: int32): int32;
var
	di, mo: int32;
begin
	di := x div (a+c);
	mo := x mod (a+c);
	mo := min(mo, a);
	result := b*a * di + b * mo;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b, c, d, e, f, x);

	tak := dist(a, b, c, x);
	aok := dist(d, e, f, x);

	if tak > aok then
		writeln('Takahashi')
	else if tak < aok then
		writeln('Aoki')
	else
		writeln('Draw');
end.
