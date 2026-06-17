program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #palindrome #base
const
	EE = 40;
var
	x: int32;
	n, y, ans: int64;
	a, e, m: int8;
	d10, da: array [0 .. EE] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function isapal(y: int64): boolean;
var
	e, l, r: int8;
begin
	e := 0;
	while y > 0 do begin
		da[e] := y mod a;
		y := y div a;
		inc(e);
	end;

	l := 0;
	r := e-1;
	while (l < r) and (da[l] = da[r]) do begin
		inc(l);
		dec(r);
	end;
	result := l >= r;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a);
	readln(n);

	ans := 0;
	for x := 1 to 1000 * 1000 - 1 do begin

		y := x;
		e := 0;
		while y > 0 do begin
			d10[e] := y mod 10;
			y := y div 10;
			inc(e);
		end;
		m := e-1;

		y := x;
		for e := 1 to m do
			y := 10 * y + d10[e];
		if (y <= n) and isapal(y) then inc(ans, y);

		y := x;
		for e := 0 to m do
			y := 10 * y + d10[e];
		if (y <= n) and isapal(y) then inc(ans, y);

	end;

	writeln(ans);
end.
