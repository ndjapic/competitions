program D2_Red_Light_Green_Light_Hard_version;
const
	nn = 200 * 1000 + 1;
	inf = 1000 * 1000 * 1000 * 1000 * 1000 + 1;
var
    ntc, tci, n, i, q, j: int16;
    k, x, t: int64;
    ans, dir: boolean;
    p, d: array [0 .. nn] of int64;
    seen: array [0 .. nn, boolean] of boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);
        for i := 1 to n do read(p[i]); readln;
        for i := 1 to n do read(d[i]); readln;

		p[0] := 0;
		p[n+1] := inf;
		d[0] := 0;
		d[n+1] := 0;

        readln(q);

        for j := 1 to q do begin
			read(x);

			for i := 0 to n+1 do begin
				seen[i, false] := false;
				seen[i, true] := false;
			end;

			ans := true;
			dir := true;
			t := 0;
			i := 1;
			while (i <= n) and (p[i] < x) do inc(i);

			while ans and (x > 0) and (x < inf) do begin

				if dir then begin

					inc(t, p[i]-x);
					x := p[i];
					if t mod k <> d[i] then
						inc(i)
					else begin
						ans := not seen[i, dir];
						seen[i, dir] := true;
						dir := not dir;
						dec(i);
					end;

				end else begin

					inc(t, x-p[i]);
					x := p[i];
					if t mod k <> d[i] then
						dec(i)
					else begin
						ans := not seen[i, dir];
						seen[i, dir] := true;
						dir := not dir;
						inc(i);
					end;

				end;

			end;

			if ans then
				writeln('YES')
			else
				writeln('NO');

        end;
        readln;

    end;
end.
