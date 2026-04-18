# Задатак: A_2_n_2_n.pas

```pascal
program A_2_n_2_n;
var
	n, ans: int32;

begin
	readln(n);
	ans := 1 shl n - 2*n;
	writeln(ans);
end.

```
