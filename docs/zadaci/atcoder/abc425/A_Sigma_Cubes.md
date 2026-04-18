# Задатак: A_Sigma_Cubes.pas

```pascal
program A_Sigma_Cubes;
var
	n, i, cube, s: int32;

begin
	readln(n);

	s := 0;
	for i := 1 to n do begin
		cube := i*i*i;
		if odd(i) then
			dec(s, cube)
		else
			inc(s, cube);
	end;

	writeln(s);
end.

```
