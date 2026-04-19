# Problem: E_Cover_query.pas

```pascal
program E_Cover_query;
const
	nn = 200 * 1000;
var
	n, i: int32;
	p: array [1 .. nn] of int32;

begin
	readln(n);

	for i := 1 to n do readln(p[i]);
	readln;
end.

```
