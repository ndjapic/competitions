# Problem: C_Palindromic_in_Both_Bases.pas

```pascal
program C_Palindromic_in_Both_Bases;
{$INLINE ON}
var
	a, i, k: int8;
	n, x, y, op, ep, ans: int64;
	d: array [1 .. 40] of int8;

function ispal(x: int64): boolean; inline;
var
	k, l, r: int8;
begin
	k := 0;
	while x > 0 do begin
		inc(k);
		d[k] := x mod a;
		x := x div a;
	end;

	l := 1;
	r := k;
	while (l < r) and (d[l] = d[r]) do begin
		inc(l);
		dec(r);
	end;

	ispal := l >= r;
end;

begin
	readln(a);
	readln(n);

	ans := 0;
	for x := 1 to 999 * 1001 do begin

		k := 0;
		y := x;
		while y > 0 do begin
			inc(k);
			d[k] := y mod 10;
			y := y div 10;
		end;

		op := x;
		ep := x*10 + d[1];
		for i := 2 to k do begin
			op := op*10 + d[i];
			ep := ep*10 + d[i];
		end;

		if (op <= n) and ispal(op) then inc(ans, op);
		if (ep <= n) and ispal(ep) then inc(ans, ep);
	end;

	writeln(ans);
end.

```
