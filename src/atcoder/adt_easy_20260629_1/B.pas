program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #error
uses
	math;
var
	x: double;
	a, b, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x);

	a := floor(x);
	b := ceil(x);

	if a = b then
		ans := a
	else if x-a < b-x then
		ans := a
	else if x-a > b-x then
		ans := b
	else if odd(b) then
		ans := b
	else
		ans := b;

	writeln(ans);
end.
