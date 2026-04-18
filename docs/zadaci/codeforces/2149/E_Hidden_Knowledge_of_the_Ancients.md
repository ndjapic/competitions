# Задатак: E_Hidden_Knowledge_of_the_Ancients.pas

```pascal
program E_Hidden_Knowledge_of_the_Ancients;
{$MODE DELPHI}
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, k, l, r, b, c, d: int32;
	ans: int64;
	a: array [1 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]); readln;

		writeln(ans);

	end;
end.

```
