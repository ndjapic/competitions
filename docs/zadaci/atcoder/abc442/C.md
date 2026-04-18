# Задатак: C.pas

```pascal
program C_;
const
	nn = 200 * 1000;
var
	n, m, i, a, b: int32;
	r: int64;
	rev : array [1 .. nn] of int32;

begin
	readln(n, m);

	for a := 1 to n do rev[a] := n-1;

	for i := 1 to m do begin
		readln(a, b);
		dec(rev[a]);
		dec(rev[b]);
	end;

	for a := 1 to n do begin
		r := rev[a];
		write(r * (r-1) * (r-2) div 6);
		if a < n then write(' ');
	end;
	writeln;
end.

```
