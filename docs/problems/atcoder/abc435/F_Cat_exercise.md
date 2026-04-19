# Problem: F_Cat_exercise.pas

```pascal
program F_Cat_exercise;
const
	nn = 200 * 1000;
var
	n, i: int32;
	p: array [1 .. nn] of int32;

begin
	readln(n);

	for i := 1 to n do readln(p[i]);
	readln;
end.

```
