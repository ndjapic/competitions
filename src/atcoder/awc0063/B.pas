program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #gcd #lcm #overflow
const
	nn = 100 * 1000;
var
	n, i, g: int32;
	m, j: int64;
	ans: boolean;
	p: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function gcd(a, b: int64): int64;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do read(p[i]);
	readln;

	j := p[1];
	i := 1;
	ans := true;

	while (i <= n) and ans do begin
		g := gcd(j, p[i]);
		j := j div g;
		if j <= m div p[i] then
			j := j * p[i]
		else
			ans := false;
		inc(i);
	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
