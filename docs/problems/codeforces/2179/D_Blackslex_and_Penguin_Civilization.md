# Problem: D_Blackslex_and_Penguin_Civilization.pas

```pascal
program D_Blackslex_and_Penguin_Civilization;
const
	pp = 1 shl 16;
var
	notc, tci, n, i, e, x, y: int32;
	p: array [0 .. pp] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		x := 1;
		y := 1 shl n;
		p[0] := y - 1;

		for e := 0 to n-1 do begin
			y := y div 2;
			for i := 0 to x-1 do
				p[x+i] := (2*i+1) * y - 1;
			x := 2 * x;
		end;

		for i := 0 to x-2 do write(p[i], ' ');
		writeln(p[x-1]);

	end;
end.

```
