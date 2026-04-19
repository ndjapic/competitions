# Problem: A_OS_Versions.pas

```pascal
program A_OS_Versions;
{$MODE DELPHI}
var
	s: string;
	i, x, y: int8;

begin
	readln(s);

	i := 1;
	case s[i] of
		'O': x := 1;
		'S': x := 2;
		'L': x := 3;
	end;

	while s[i] <> ' ' do inc(i);
	case s[i+1] of
		'O': y := 1;
		'S': y := 2;
		'L': y := 3;
	end;

	if x >= y then
		writeln('Yes')
	else
		writeln('No');
end.

```
