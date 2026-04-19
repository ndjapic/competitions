# Problem: B.pas

```pascal
program _B;
uses
	math;
const
	nn = 200 * 1000;
var
	n, t, i: int32;
	ai, bi, ans: int64;

begin
	readln(n, t);

	ans := 0;
	for i := 1 to n do begin
		readln(ai, bi);
		inc(ans, max(0, ai - bi * t));
	end;

	writeln(ans);
end.

```
