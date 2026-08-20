program sum_41_chapter_1;
uses
	math;
const
	maxn = 100;
var
    ntc, tci: int16;
    n, i: int8;
    p, d, s: int32;
    a: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(p);

		n := 0;
		d := 2;
		while d*d <= p do
			if p mod d > 0 then
				inc(d)
			else begin
				inc(n);
				a[n] := d;
				p := p div d;
			end;

		if p > 1 then begin
			inc(n);
			a[n] := p;
		end;

		s := 0;
		for i := 1 to n do inc(s, a[i]);

		if s > 41 then
			n := -1
		else
			while s < 41 do begin
				inc(n);
				a[n] := 1;
				inc(s);
			end;

		write('Case #', tci, ': ', n);
		for i := 1 to n do write(' ', a[i]);
		writeln;

    end;
end.
