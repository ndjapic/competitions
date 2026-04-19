# Problem: A_El_fucho.pas

```pascal
program A_El_fucho;
var
	notc, tci, n, w, l, hw, hl, matches: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		matches := 1;
		w := n;
		l := 0;

		while (w > 1) or (l > 1) do begin

			hw := w div 2;
			hl := l div 2;

			inc(matches, hw);
			inc(matches, hl);

			dec(l, hl);
			inc(l, hw);
			dec(w, hw);

		end;

		writeln(matches);

	end;
end.

```
