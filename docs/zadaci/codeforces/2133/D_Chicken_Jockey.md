# Задатак: D_Chicken_Jockey.pas

```pascal
program D_Chicken_Jockey;
uses
	math;
const
	nn = 200 * 1000;
var
	ntc, tci: int16;
	n, i, j, d: int32;
	ans: int64;
	loop: boolean;
	h: array [1 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		for i := 1 to n do read(h[i]);
		readln;

		ans := 0;
		d := 0;
		for i := n downto 1 do
			if h[i] > 1 then begin
				inc(ans, h[i]);
				j := i+1;
				loop := true;
				while (j <= n) and loop do begin
					dec(h[j], min(n+0-j, h[j]));
					loop := h[j] = 0;
					inc(j);
				end;
			end;

		writeln(ans);

	end;
end.

```
