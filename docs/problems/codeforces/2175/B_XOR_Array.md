# Problem: B_XOR_Array.pas

```pascal
program B_XOR_Array;
const
	nn = 400 * 1000;
var
	notc, tci, n, i, l, r: int32;
	a: array [0 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, l, r);

		for i := 0 to n do begin
			a[i] := i;
			if i = r then a[i] := a[l-1];
		end;

		for i := n downto 1 do a[i] := a[i] xor a[i-1];

		for i := 1 to n-1 do write(a[i], ' ');
		writeln(a[n]);

	end;
end.

```
