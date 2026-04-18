program E_Sum_of_Subarrays;
const
	nn = 300 * 1000;
var
	n, q, i, l, r: int32;
	a0, a1, a2, a3: array [0 .. nn] of int32;

begin
	readln(n, q);

	a1[0] := 0;
	a2[0] := 0;
	a3[0] := 0;
	for r := 1 to n do begin
		read(a0[r]);
		a1[r] := a1[r-1] + a0[r];
		a2[r] := a2[r-1] + a1[r];
		a3[r] := a3[r-1] + a2[r];
	end;
	readln;

	for i := 1 to q do begin
		readln(l, r);
		dec(l);
		writeln(a3[r] - a3[l] - (a2[r] - a2[l]) * l - a2[l] * (r-l));
	end;
end.
