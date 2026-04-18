# Задатак: A_Grandma_s_Footsteps.pas

```pascal
program A_Grandma_s_Footsteps;
uses
	math;
var
	s, a, b, x, ans: int16;

begin
	readln(s, a, b, x);

	ans := x div (a+b) * a;
	x := x mod (a+b);
	inc(ans, min(x, a));

	writeln(ans * s);
end.

```
