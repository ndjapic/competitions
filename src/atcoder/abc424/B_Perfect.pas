program B_Perfect;
var
	n, m, k, i, j, a, b: int8;
	c, ans: array [1 .. 10] of int8;

begin
	readln(n, m, k);

	for a := 1 to n do c[a] := 0;

	j := 0;
	for i := 1 to k do begin
		readln(a, b);
		inc(c[a]);
		if c[a] = m then begin
			inc(j);
			ans[j] := a;
		end;
	end;

	if j > 0 then begin
		for i := 1 to j-1 do write(ans[i], ' ');
		writeln(ans[j]);
	end;
end.
