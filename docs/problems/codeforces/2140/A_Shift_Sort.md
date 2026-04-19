# Problem: A_Shift_Sort.pas

```pascal
program A_Shift_Sort;
{$MODE DELPHI}
const
	nn = 100;
var
	ntc, tci, n, i: int8;
	s: string;
	c1: array [0 .. nn] of int8;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);
		readln(s);

		c1[0] := 0;
		for i := 1 to n do
			c1[i] := c1[i-1] + ord(s[i]) - ord('0');

		writeln(c1[n - c1[n]]);

	end;
end.

```
