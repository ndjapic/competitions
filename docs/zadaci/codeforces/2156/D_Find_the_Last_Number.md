# Задатак: D_Find_the_Last_Number.pas

```pascal
program D_Find_the_Last_Number;
const
	nn = 20 * 1000;
var
	notc, tci, n, i, x, c: int32;
	b, e: int8;
	p: array [1 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do p[i] := 0;

		e := 0;
		while n shr e > 0 do begin
			c := 0;
			for x := 1 to n do
				if x and ((1 shl e) - 1) = p[n] then
					inc(c, (x shr e) and 1);
			for i := 1 to n-1 do begin
				x := 1 shl e;
				if p[i] = p[n] then begin
					writeln('? ', i, ' ', x);
					flush(output);
					readln(b);
					inc(p[i], x * b);
					dec(c, b);
				end;
				inc(p[n], x * c);
			end;
			inc(e);
		end;

		writeln('! ', p[n]);
		flush(output);

	end;
end.

```
