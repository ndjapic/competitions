# Задатак: B_Frequency.pas

```pascal
program B_Frequency;
{$H+}
const
    maxn = 200 * 1000;
var
    n, i, j: int16;
    s: string;
    c: array [1 .. 26] of int16;

begin
    readln(s);
    n := length(s);

	for i := 1 to 26 do c[i] := 0;
	for i := 1 to n do inc(c[ord(s[i]) and 31]);

	j := 0;
	for i := 1 to 26 do
		if c[i] > c[j] then j := i;

	writeln(chr(ord('a') - 1 + j));
end.

```
