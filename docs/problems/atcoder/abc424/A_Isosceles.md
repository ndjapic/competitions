# Problem: A_Isosceles.pas

```pascal
program A_Isosceles;
var
	a, b, c: int8;

begin
	readln(a, b, c);

	if a = b then
		writeln('Yes')
	else if b = c then
		writeln('Yes')
	else if c = a then
		writeln('Yes')
	else
		writeln('No');
end.

```
