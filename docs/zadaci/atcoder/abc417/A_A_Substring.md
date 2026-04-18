# Задатак: A_A_Substring.pas

```pascal
program A_A_Substring;
{$MODE DELPHI}
var
	n, i, a, b: int8;
	s: string;

begin
	readln(n, a, b);
	readln(s);

	for i := 1+a to n-b do write(s[i]);
	writeln;
end.

```
