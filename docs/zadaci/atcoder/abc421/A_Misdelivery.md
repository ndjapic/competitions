# Задатак: A_Misdelivery.pas

```pascal
program A_Misdelivery;
{$MODE DELPHI}
const
	nn = 100;
var
	n, i, x: int32;
	blank: char;
	s: array [1 .. 100] of string;
	y: string;

begin
	readln(n);

	for i := 1 to n do readln(s[i]);
	read(x);
	read(blank);
	readln(y);

	if s[x] = y then
		writeln('Yes')
	else
		writeln('No');
end.

```
