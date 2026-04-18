# Задатак: D.pas

```pascal
program _D;
{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, k, i, l, r: int32;
	m, s, ans: int64;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k, m);

	for i := 1 to n do read(a[i]);
	readln;

	ans := 0;
	l := 1;
	r := 0;
	s := 0;

	while r <= n do
		if (r-l+1 >= k) and (s >= m) then begin
			inc(ans, n+1-r);
			dec(s, a[l]);
			inc(l);
		end else begin
			inc(r);
			if r <= n then inc(s, a[r]);
		end;

	writeln(ans);
end.

```
