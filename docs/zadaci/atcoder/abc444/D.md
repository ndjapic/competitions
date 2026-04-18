# Задатак: D.pas

```pascal
program _D;
const
	nn = 200 * 1000;
var
	n, i, e: int32;
	c: array of int64;

begin
	readln(n);

	setlength(c, nn);
	for e := 0 to nn-1 do c[e] := 0;

	for i := 1 to n do begin
		read(e);
		inc(c[e-1]);
	end;
	readln;

	for e := nn downto 1 do inc(c[e-1], c[e]);

	e := 0;
	while c[e] > 0 do begin
		if e+1 >= length(c) then setlength(c, 2 * (e+1));
		c[e+1] := c[e+1] + c[e] div 10;
		c[e] := c[e] mod 10;
		inc(e);
	end;

	while e > 0 do begin
		dec(e);
		write(c[e]);
	end;
	writeln;
end.

```
