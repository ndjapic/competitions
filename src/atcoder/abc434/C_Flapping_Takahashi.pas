program C_Flapping_Takahashi;
uses
	math;
const
	nn = 100 * 1000;
var
	notc, tci, n, i, h, d: int32;
	t, l, u: array [0 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin
		readln(n, h);

		for i := 1 to n do readln(t[i], l[i], u[i]);

		i := 0;
		t[0] := 0;
		l[0] := h;
		u[0] := h;

		while (i < n) and (l[i] <= u[i]) do begin
			inc(i);
			d := t[i] - t[i-1];
			l[i] := max(l[i], l[i-1] - d);
			u[i] := min(u[i], u[i-1] + d);
		end;

		if l[i] <= u[i] then
			writeln('Yes')
		else
			writeln('No');
	end;
end.
