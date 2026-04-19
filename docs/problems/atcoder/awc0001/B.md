# Problem: B.pas

```pascal
program _B;
const
	nn = 200 * 1000;
var
	n, i, l, r, ans: int32;
	p: array [1 .. nn] of int8;

begin
	readln(n, l, r);

	ans := -1;
	for i := 1 to n do begin
		read(p[i]);
		if (l <= p[i]) and (p[i] <= r) then
			if (ans = -1) or (p[i] > p[ans]) then ans := i;
	end;
	readln;

	writeln(ans);
end.

```
