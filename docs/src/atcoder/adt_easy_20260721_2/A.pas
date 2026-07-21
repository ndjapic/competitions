program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	a, b, c: int8;
	ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function num(a, b, c: int8): int32;
begin
	Result := 100 * a + 10 * b + c;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b, c);

	if a > b then begin
		if b > c then
			ans := num(a, b, c)
		else if c > a then
			ans := num(c, a, b)
		else
			ans := num(a, c, b)
	end else begin
		if a > c then
			ans := num(b, a, c)
		else if c > b then
			ans := num(c, b, a)
		else
			ans := num(b, c, a)
	end;

	writeln(ans);
end.
