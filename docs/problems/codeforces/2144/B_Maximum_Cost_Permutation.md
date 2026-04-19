# Problem: B_Maximum_Cost_Permutation.pas

```pascal
program B_Maximum_Cost_Permutation;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, x, l, r: int32;
	p: array [1 .. nn] of int32;
	seen: array [1 .. nn] of boolean;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for x := 1 to n do seen[x] := false;

		for i := 1 to n do begin
			read(x);
			p[i] := x;
			if x > 0 then
				seen[x] := true;
		end;
		readln;

		x := n;
		for i := 1 to n do
			if p[i] = 0 then begin
				while seen[x] do dec(x);
				p[i] := x;
				seen[x] := true;
			end;

		l := 1;
		r := n;
		while (l <= r) and (p[l] = l) do inc(l);
		while (l <= r) and (p[r] = r) do dec(r);

		writeln(r-l+1);

	end;
end.

```
