program B_Even_Modulo_Pair;
const
	nn = 100 * 1000;
var
	notc, tci, n, i, j, x, y: int32;
	a: array [1 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		x := -1;

		for i := 1 to n do begin
			read(a[i]);
			j := 1;
			while (x = -1) and (j < i) and odd(a[i] mod a[j]) do
				inc(j);
			if (x = -1) and (j < i) then begin
				x := a[j];
				y := a[i];
			end;
		end;
		readln;

		if (x = -1) then
			writeln(-1)
		else
			writeln(x, ' ', y);

	end;
end.
