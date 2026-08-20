program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, i: int32;
	t1, ti, p: int64;
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
	lcm := a div gcd(a, b) * b;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	read(t1);

	p := 1;
	for i := 2 to n do begin
		read(ti);
		p := lcm(p, ti div gcd(t1, ti));
	end;
	readln;

	writeln(p, '/1');
end.
