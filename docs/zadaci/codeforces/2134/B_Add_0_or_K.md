# Задатак: B_Add_0_or_K.pas

```pascal
program B_Add_0_or_K;
uses
	math;
const
	nn = 100 * 1000;
	p: array [1 .. 10] of int32 = (2,3,5,7,11,13,17,19,23,29);
var
	ntc, tci: int16;
	n, k, i: int32;
	j: int8;
	a: array [1 .. nn] of int64;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, k);

		j := 1;
		while k mod p[j] = 0 do inc(j);

		for i := 1 to n do begin
			read(a[i]);
			while a[i] mod p[j] > 0 do inc(a[i], k);
		end;
		readln;

		for i := 1 to n-1 do write(a[i], ' ');
		writeln(a[n]);

	end;
end.

```
