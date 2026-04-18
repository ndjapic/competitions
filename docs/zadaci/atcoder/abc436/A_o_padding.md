# Задатак: A_o_padding.pas

```pascal
program A_o_padding;
var
	n, m, i: int32;
	s: string;

begin
	readln(n);
	readln(s);
	m := length(s);

	for i := 1 to n-m do write('o');
	writeln(s);
end.

```
