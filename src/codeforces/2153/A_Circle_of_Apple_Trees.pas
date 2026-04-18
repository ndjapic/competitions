program A_Circle_of_Apple_Trees;
const
	nn = 100;
var
	notc, tci, n, i, bi, ans: int32;
	seen: array [1 .. nn] of boolean;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for bi := 1 to n do seen[bi] := false;

		ans := 0;
		for i := 1 to n do begin
			read(bi);
			if not seen[bi] then begin
				seen[bi] := true;
				inc(ans);
			end;
		end;
		readln;

		writeln(ans);

	end;
end.
