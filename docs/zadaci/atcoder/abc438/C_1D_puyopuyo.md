# Задатак: C_1D_puyopuyo.pas

```pascal
program C_1D_puyopuyo;
uses
	math;
const
	nn = 200 * 1000 + 1;
var
	n, i, ln, rn: int32;
	loop: boolean;
	a: array [1 .. nn] of int32;
	ls, rs: array [0 .. nn] of int32;

begin
	readln(n);

	for i := 1 to n do begin
		read(a[i]);
	end;
	readln;

	ln := 0;
	rn := 0;
	for i := n downto 1 do begin
		inc(rn);
		rs[rn] := a[i];
	end;

	loop := true;
	while loop do
		if (ln >= 4) and (ls[ln] = ls[ln-1]) and (ls[ln] = ls[ln-2]) and (ls[ln] = ls[ln-3]) then begin
			dec(ln, 4);
			dec(n, 4);
		end else if rn > 0 then begin
			inc(ln);
			ls[ln] := rs[rn];
			dec(rn);
		end else
			loop := false;

	writeln(n);
end.

```
