# Задатак: B_Tombola.pas

```pascal
program B_Tombola;
uses
	math;
var
	h, w, i, j, k, n, b, ans: int32;
	a: array [1 .. 3, 1 .. 5] of int32;
	c: array [1 .. 3] of int32;

begin
	readln(h, w, n);

	for i := 1 to h do begin
		c[i] := 0;
		for j := 1 to w do read(a[i, j]);
		readln;
	end;

	for k := 1 to n do begin
		read(b);
		for i := 1 to h do begin
			j := 1;
			while (j <= w) and (a[i, j] <> b) do inc(j);
			if j <= w then inc(c[i]);
		end;
	end;

	ans := 0;
	for i := 1 to h do ans := max(ans, c[i]);
	writeln(ans);
end.

```
