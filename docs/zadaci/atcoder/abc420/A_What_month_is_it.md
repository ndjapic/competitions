# Задатак: A_What_month_is_it.pas

```pascal
program A_What_month_is_it;
var
    x, y: int8;

begin
	readln(x, y);
	inc(x, y);
	if x > 12 then dec(x, 12);
	writeln(x);
end.

```
