# Задатак: B_Permute_to_Minimize.pas

```pascal
program B_Permute_to_Minimize;
var
	x: int32;
	d: int8;
	c: array [0 .. 9] of int8;

begin
	readln(x);

	for d := 0 to 9 do c[d] := 0;

	while x > 0 do begin
		inc(c[x mod 10]);
		x := x div 10;
	end;

	x := 1;
	while c[x] = 0 do inc(x);
	dec(c[x]);

	for d := 0 to 9 do
		while c[d] > 0 do begin
			x := 10 * x + d;
			dec(c[d]);
		end;

	writeln(x);
end.

```
