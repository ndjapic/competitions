program C_2026;
uses
	math;
const
	nn = 10 * 1000 * 1000;
var
	n, i, k, x, y, z: int32;
	c: array [1 .. nn] of int32;
	a: array of int32;

function isqrt(a: int64): int64;
var
    x: int64;
begin
    x := min(a, high(int32));
    while x * x > a do
        x := (x + a div x) div 2;
    isqrt := x;
end;

begin
	readln(n);

	for z := 1 to n do c[z] := 0;

	if n > 1 then
		for y := 2 to isqrt(n-1) do
			for x := 1 to min(isqrt(n - sqr(y)), y-1) do begin
				inc(c[sqr(x) + sqr(y)]);
			end;

	k := 0;
	setlength(a, 1);
	for z := 1 to n do
		if c[z] = 1 then begin
			if length(a) = k then setlength(a, 2*k);
			a[k] := z;
			inc(k);
		end;

	writeln(k);
	for i := 0 to k-2 do write(a[i], ' ');
	if k > 0 then write(a[k-1]);
	writeln;
end.
