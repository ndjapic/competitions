# Задатак: D.pas

```pascal
program _D;
uses
	math;
const
	nn = 300 * 1000;
var
	notc, tci, n, i: int32;
	ans: int64;
	r, l: array [1 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		for i := 1 to n do begin
			read(r[i]);
			l[i] := r[i];
		end;
		readln;

		for i := 2 to n do
			l[i] := min(l[i], l[i-1] + 1);

		for i := n-1 downto 1 do
			l[i] := min(l[i], l[i+1] + 1);

		ans := 0;
		for i := 1 to n do inc(ans, r[i] - l[i]);
		writeln(ans);

	end;
end.

```
