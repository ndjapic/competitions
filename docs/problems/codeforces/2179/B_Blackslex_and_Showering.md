# Problem: B_Blackslex_and_Showering.pas

```pascal
program B_Blackslex_and_Showering;
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, k, ans: int32;
	a, d: array [1 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		read(a[1]);
		d[1] := 0;
		for i := 2 to n do begin
			read(a[i]);
			d[i] := d[i-1] + abs(a[i] - a[i-1]);
		end;
		readln;

		ans := min(d[n] - d[2], d[n-1]);
		for k := 2 to n-1 do
			ans := min(ans, d[k-1] + abs(a[k-1] - a[k+1]) + d[n] - d[k+1]);

		writeln(ans);

	end;
end.

```
