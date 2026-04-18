# Задатак: E_Minimum_Swap.pas

```pascal
program E_Minimum_Swap;
const
	nn = 300 * 1000;
var
	n, i, j, c: int32;
	ans: int64;
	p: array [1 .. nn] of int32;
	seen: array [1 .. nn] of boolean;

begin
	readln(n);

	for i := 1 to n do begin
		read(p[i]);
		seen[i] := false;
	end;
	readln;

	ans := 0;
	for i := 1 to n do begin
		c := 0;
		j := i;
		while not seen[j] do begin
			seen[j] := true;
			j := p[j];
			inc(ans, c);
			inc(c);
		end;
	end;

	writeln(ans);
end.

```
