# Problem: B_Pathless.pas

```pascal
program B_Pathless;
var
	ntc, tci, n, i, s, t: int16;
	a: array [1 .. 50] of int8;
	c: array [0 .. 2] of int8;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, s);

		t := 0;
		c[0] := 0;
		c[1] := 0;
		c[2] := 0;

		for i := 1 to n do begin
			read(a[i]);
			inc(t, a[i]);
			inc(c[a[i]]);
		end;
		readln;

		if s = t then
			writeln(-1)
		else if s >= t+2 then
			writeln(-1)
		else begin

			i := 0;

			while c[0] > 0 do begin
				inc(i);
				a[i] := 0;
				dec(c[0]);
			end;

			while c[2] > 0 do begin
				inc(i);
				a[i] := 2;
				dec(c[2]);
			end;

			while c[1] > 0 do begin
				inc(i);
				a[i] := 1;
				dec(c[1]);
			end;

			for i := 1 to n-1 do write(a[i], ' ');
			writeln(a[n]);

		end;

	end;
end.

```
