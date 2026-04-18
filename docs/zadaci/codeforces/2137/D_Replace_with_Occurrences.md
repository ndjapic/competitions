# Задатак: D_Replace_with_Occurrences.pas

```pascal
program D_Replace_with_Occurrences;
var
	ntc, tci, n, i, x: int32;
	b, f: array of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);
		setlength(b, n+1);
		setlength(f, n+1);

		for x := 1 to n do f[x] := 0;

		for i := 1 to n do begin
			read(x);
			b[i] := x;
			inc(f[x]);
		end;
		readln;

		i := 1;
		while (i <= n) and (f[b[i]] = b[i]) do inc(i);

		if i <= n then
			writeln(-1)
		else begin

			for i := 1 to n-1 do write(b[i], ' ');
			writeln(b[n]);

		end;

	end;
end.

```
