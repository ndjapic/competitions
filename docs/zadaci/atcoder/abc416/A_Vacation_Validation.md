# Задатак: A_Vacation_Validation.pas

```pascal
program A_Vacation_Validation;
{$MODE DELPHI}
var
	n, l, r, i: int8;
	s: string;

begin
	readln(n, l, r);
	readln(s);

	i := l;
	while (i <= r) and (s[i] = 'o') do inc(i);

	if i <= r then
		writeln('No')
	else
		writeln('Yes');
end.

```
