program B_Search_and_Delete;
var
	n, m, i, j: int8;
	b: int32;
	a: array [1 .. 100] of int32;

begin
	readln(n, m);

	for i := 1 to n do read(a[i]); readln;

	for j := 1 to m do begin
		read(b);
		i := n;
		while (i > 0) and (a[i] > b) do dec(i);

		if (i > 0) and (a[i] = b) then begin
			while i < n do begin
				a[i] := a[i+1];
				inc(i);
			end;
			dec(n);
		end;
	end;
	readln;

	for i := 1 to n-1 do write(a[i], ' ');
	if n > 0 then writeln(a[n]);
end.
