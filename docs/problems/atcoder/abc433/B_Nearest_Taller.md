# Problem: B_Nearest_Taller.pas

```pascal
program B_Nearest_Taller;
const
	nn = 100;
var
	n, i, j: int8;
	a: array [1 .. nn] of int8;

begin
	readln(n);

	for i := 1 to n do begin
		read(a[i]);

		j := i-1;
		while (j > 0) and (a[j] <= a[i]) do dec(j);

		if j = 0 then j := -1;
		writeln(j);
	end;
	readln;

end.

```
