# Problem: C_Maximum_GCD_on_Whiteboard.pas

```pascal
program C_Maximum_GCD_on_Whiteboard;
const
	nn = 200 * 1000;
var
	notc, tci, n, k, i, x: int32;
	m: int8;
	a, c: array [0 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);

		for x := 0 to n do c[x] := 0;

		for i := 1 to n do begin
			read(x);
			a[i] := x;
			inc(c[x div 4 + 1]);
		end;
		readln;

		for x := 1 to n do inc(c[x], c[x-1]);

		for i := 1 to n do begin
			x := a[i];
			for m := 1 to 3 do
				if x mod m = 0 then dec(c[x div m]);
		end;

		x := n;
		while c[x] > k do dec(x);
		writeln(x);

	end;
end.

```
