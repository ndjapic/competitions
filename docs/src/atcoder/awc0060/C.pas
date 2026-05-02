program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int32;
	a, b, p1, q1, p2, q2, g: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function gcd(a, b: int64): int64;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

function lcm(a, b: int64): int64;
begin
    lcm := int64(a) div gcd(a, b) * b;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);

	readln(a, b);
	for i := 2 to n do begin
		p1 := a;
		q1 := b;
		readln(p2, q2);

		b := gcd(q1, q2);
		g := gcd(p1, p2);
		a := lcm(p1 div g * p2, p2 div g * p1);

		g := gcd(a, b);
		a := a div g;
		b := b div g;
	end;

	writeln(a, ' ', b);
end.
