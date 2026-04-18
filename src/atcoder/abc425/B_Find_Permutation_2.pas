program B_Find_Permutation_2;
const
	nn = 10;
var
	n, i, x: int8;
	ans: boolean;
	a: array [1 .. nn] of int8;
	seen: array [1 .. nn] of boolean;

begin
	readln(n);

	for x := 1 to n do seen[x] := false;
	ans := true;

	for i := 1 to n do begin
		read(x);
		a[i] := x;
		if x > -1 then begin
			ans := ans and not seen[x];
			seen[x] := true;
		end;
	end;
	readln;

	if ans then begin
		x := 1;

		for i := 1 to n do
			if a[i] = -1 then begin
				while seen[x] do inc(x);
				a[i] := x;
				seen[x] := true;
			end;

		writeln('Yes');
		for i := 1 to n-1 do write(a[i], ' ');
		writeln(a[n]);

	end else
		writeln('No');
end.
