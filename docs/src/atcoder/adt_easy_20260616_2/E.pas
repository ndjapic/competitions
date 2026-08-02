program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #bisect #palindrome #cube
var
	n, k, l, r, x: int64;
	d: array [0 .. 18] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function pal(x: int64): boolean;
var
	e, l, r: int8;
begin
	e := 0;
	while x > 0 do begin
		d[e] := x mod 10;
		x := x div 10;
		inc(e);
	end;

	l := 0;
	r := e-1;
	while (l < r) and (d[l] = d[r]) do begin
		inc(l);
		dec(r);
	end;
	result := l >= r;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	l := 1;
	r := 1000 * 1000 + 1;
	while r-l > 1 do begin
		x := (r+l) div 2;
		if x*x*x > n then
			r := x
		else
			l := x;
	end;

	x := r;
	repeat
		dec(x);
		k := x*x*x;
	until pal(k);
	writeln(k);
end.
