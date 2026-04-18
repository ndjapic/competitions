# Задатак: B.pas

```pascal
program _B;
{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci: int32;
	d, ans: int8;
	s: int16;
	x: int64;
	c: array [0 .. 9] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(x);

		for d := 0 to 9 do c[d] := 0;

		s := 0;
		while x > 9 do begin
			d := x mod 10;
			inc(c[d]);
			inc(s, d);
			x := x div 10;
		end;
		inc(c[x-1]);
		inc(s, x-1);

		ans := 0;
		d := 9;
		while s > 8 do
			if c[d] = 0 then
				dec(d)
			else begin
				dec(s, d);
				dec(c[d]);
				inc(ans);
			end;

		writeln(ans);

	end;
end.

```
