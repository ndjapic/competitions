program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, i, x, y, g, p: int32;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function gcd(a, b: int32): int32;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, x, y);
		g := gcd(x, y);
		ans := true;

		for i := 1 to n do begin
			read(p);
			if ans then ans := p mod g = i mod g;
		end;
		readln;

		if ans then
			writeln('YES')
		else
			writeln('NO');

	end;
end.
