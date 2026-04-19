# Problem: B_The_Odd_One_Out.pas

```pascal
program B_The_Odd_One_Out;
{$MODE DELPHI}
var
	s: string;
	n, i, o: int8;
	c: array [1 .. 26] of int8;

begin
	readln(s);
	n := length(s);

	for o := 1 to 26 do c[o] := 0;

	for i := 1 to n do begin
		o := ord(s[i]) - 96;
		inc(c[o]);
	end;

	o := 1;
	while c[o] <> 1 do inc(o);

	writeln(chr(96 + o));
end.

```
