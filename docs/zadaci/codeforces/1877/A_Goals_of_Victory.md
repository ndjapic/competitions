# Задатак: A_Goals_of_Victory.pas

```pascal
program A_Goals_of_Victory;
const
	maxn = 100;
var
    ntc, tci: int16;
    n, i: int8;
    a: array [1 .. maxn] of int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

		a[n] := 0;
		for i := 1 to n-1 do begin
			read(a[i]);
			dec(a[n], a[i]);
		end;
		readln;

		writeln(a[n]);

    end;
end.

```
