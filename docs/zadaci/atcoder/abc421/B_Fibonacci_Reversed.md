# Задатак: B_Fibonacci_Reversed.pas

```pascal
program B_Fibonacci_Reversed;
{$MODE DELPHI}
const
	nn = 10;
var
	i: int8;
	a: array [1 .. nn] of int64;

begin
	readln(a[1], a[2]);

	for i := 3 to nn do begin
		inc(a[i-2], a[i-1]);
		a[i] := 0;
		while a[i-2] > 0 do begin
			a[i] := a[i] * 10 + a[i-2] mod 10;
			a[i-2] := a[i-2] div 10;
		end;
	end;

	writeln(a[nn]);
end.

```
