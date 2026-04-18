# Задатак: B_Fun_Permutation.pas

```pascal
program B_Fun_Permutation;
var
	ntc, tci, n, i, x: int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		for i := 1 to n do begin
			read(x);
			write(n+1-x, ' ');
		end;
		readln;
		writeln;

	end;
end.

```
