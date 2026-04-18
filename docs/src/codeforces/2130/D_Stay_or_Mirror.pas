program D_Stay_or_Mirror;
{$MODE OBJFPC}{$h+}
uses
	math;
const
	nn = 5000;
type
	generic TR<_T> = class // D_Stay_or_Mirror.pas(7,4) Fatal: Syntax error, "=" expected but "<" found
		x, y: _T;
	end;
var
	ntc, tci, n, i, j: int32;
	inv00, inv01, inv10, inv11: int64;
	p, a: array [1 .. nn] of int32;
	mn: array [0 .. nn] of int64;
	r: specialize TR<int32>;

begin
	r := specialize TR<int32>.Create;
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		mn[0] := 0;
		for i := 1 to n do begin

			read(p[i]);
			a[i] := p[i];

			inv00 := mn[i-1];
			inv01 := mn[i-1];
			inv10 := int64(i-2) * (i-1) div 2 - mn[i-1];
			inv11 := int64(i-2) * (i-1) div 2 - mn[i-1];

			for j := 1 to i-1 do begin
				if a[j] > p[i] then inc(inv00);
				if a[j] > 2*n - p[i] then inc(inv01);
				if 2*n - a[j] > p[i] then inc(inv10);
				if 2*n - a[j] > 2*n - p[i] then inc(inv11);
			end;

			inv10 := int64(i-1) * i div 2 - inv10;
			inv11 := int64(i-1) * i div 2 - inv11;

			mn[i] := min(
				min(inv00, inv01),
				min(inv10, inv11)
			);

			if mn[i] = inv00 then
				a[i] := p[i]
			else if mn[i] = inv01 then
				a[i] := 2*n - p[i]
			else if mn[i] = inv10 then begin
				for j := 1 to i-1 do a[j] := 2*n - a[j];
				a[i] := p[i];
			end else if mn[i] = inv11 then begin
				for j := 1 to i-1 do a[j] := 2*n - a[j];
				a[i] := 2*n - p[i];
			end;

		end;
		readln;

		writeln(mn[n]);

	end;
end.
