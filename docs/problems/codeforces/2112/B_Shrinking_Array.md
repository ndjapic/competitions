# Problem: B_Shrinking_Array.pas

```pascal
program B_Shrinking_Array;
const
	nn = 1000 * 1000;
var
    ntc, tci, n, i: int16;
    d: int32;
    a: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

		for i := 1 to n do read(a[i]); readln;

		i := 1;
		while (i < n) and (abs(a[i] - a[i+1]) > 1) do inc(i);

		if i < n then
			writeln(0)
		else begin

            if a[1] < a[2] then
                d := 1
            else
                d := -1;

			i := 2;
			while (i < n) and (d * (a[i+1] - a[i]) > 0) do inc(i);

			if i < n then
				writeln(1)
			else
				writeln(-1);

		end;

    end;
end.

```
