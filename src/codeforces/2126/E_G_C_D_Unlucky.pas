program E_G_C_D_Unlucky;
const
	nn = 100 * 1000;
	inf = int64(1) shl 30;
var
	ntc, tci, n, i: int32;
	ans: boolean;
	p, s: array [1 .. nn] of int32;
	a: array [1 .. nn] of int64;

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
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		for i := 1 to n do read(p[i]); readln;
		for i := 1 to n do read(s[i]); readln;
		for i := 1 to n do a[i] := lcm(p[i], s[i]);

		ans := (a[1] = p[1]) and (a[n] = s[n]);

		if ans then begin
			i := 2;
			while (i <= n) and (p[i] = gcd(p[i-1], a[i])) do inc(i);
			ans := i > n;
		end;

		if ans then begin
			i := n-1;
			while (i > 0) and (s[i] = gcd(a[i], s[i+1])) do dec(i);
			ans := i = 0;
		end;

		if ans then
			writeln('YES')
		else
			writeln('NO');

	end;
end.
