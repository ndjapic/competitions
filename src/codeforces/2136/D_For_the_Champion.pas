program D_For_the_Champion;
{$MODE DELPHI}
uses
	math;
const
	nn = 100;
	kk = 1000 * 1000 * 1000;
var
	ntc, tci, n, i: int32;
	mxs, mxd, s, d1, d2, ansx, ansy: int64;
	x, y: array [1 .. nn] of int32;

function query(ch: char; k: int64): int64;
begin
	writeln('? ', ch, ' ', k);
	flush(output);
	readln(Result);
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		mxs := - kk * 2;
		mxd := - kk * 2;

		for i := 1 to n do begin
			readln(x[i], y[i]);
			mxs := max(mxs, x[i] + y[i]);
			mxd := max(mxd, x[i] - y[i]);
		end;

		s := query('U', kk);
		s := query('U', kk);
		s := query('R', kk);
		s := query('R', kk);

		d1 := s + mxs - kk * 4;

		s := query('D', kk);
		s := query('D', kk);
		s := query('D', kk);
		s := query('D', kk);

		d2 := s + mxd - kk * 4;

		ansx := (d1 + d2) div 2;
		ansy := (d1 - d2) div 2;

		writeln('! ', ansx, ' ', ansy);
		flush(output);

	end;
end.
