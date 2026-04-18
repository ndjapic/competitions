# Задатак: B_Your_Name.pas

```pascal
program B_Your_Name;
{$MODE DELPHI}
var
	notc, tci: int16;
	n, i, o: int8;
	s: string;
	c: array [1 .. 26] of int8;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		for o := 1 to 26 do c[o] := 0;

		for i := 1 to n do begin
			o := ord(s[i]) and 31;
			inc(c[o]);
		end;

		for i := n+2 to n+n+1 do begin
			o := ord(s[i]) and 31;
			dec(c[o]);
		end;

		o := 1;
		while (o <= 26) and (c[o] = 0) do inc(o);

		if o <= 26 then
			writeln('NO')
		else
			writeln('YES');

	end;
end.

```
