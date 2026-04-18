# Задатак: E_Lost_Soul.pas

```pascal
program E_Lost_Soul;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci, n, i, ans: int32;
    a, b: array [1 .. nn] of int32;
    seen: array [1 .. nn] of boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]); readln;
        for i := 1 to n do read(b[i]); readln;

		i := n;
		while (i > 0) and (a[i] <> b[i]) do dec(i);
		ans := i;

		for i := 1 to n do seen[i] := false;
		i := n-1;
		seen[a[n]] := true;
		while (i > 0) and not seen[a[i]] do begin
			seen[b[i+1]] := true;
			seen[a[i]] := true;
			dec(i);
		end;
		ans := max(ans, i);

		for i := 1 to n do seen[i] := false;
		i := n-1;
		seen[b[n]] := true;
		while (i > 0) and not seen[b[i]] do begin
			seen[a[i+1]] := true;
			seen[b[i]] := true;
			dec(i);
		end;
		ans := max(ans, i);

        writeln(ans);

    end;
end.

```
