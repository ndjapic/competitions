program B_Happy_Number;
var
	n, i, x: int32;
	link: array [1 .. 2026] of int32;

begin
	readln(n);

	for i := 1 to 2026 do begin
		x := i;
		link[i] := 0;
		while x > 0 do begin
			inc(link[i], sqr(x mod 10));
			x := x div 10;
		end;
	end;

	for i := 1 to 2026 do n := link[n];

	if n = 1 then
		writeln('Yes')
	else
		writeln('No');
end.
