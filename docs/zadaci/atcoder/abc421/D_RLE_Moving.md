# Задатак: D_RLE_Moving.pas

```pascal
program D_RLE_Moving;
uses
	math;
const
	mm = 100 * 1000;
var
	rt, ct, ra, ca, n: int64;
	m, l, i, j, mn, ans: int32;
	s, t: string;
	a, b: array [1 .. nn] of int32;

begin
	readln(rt, ct, ra, ca);
	readln(n, m, l);

	setlength(s, m);
	setlength(t, l);

	for i := 1 to m do begin
		read(s[i]);
		readln(a[i]);
	end;

	for j := 1 to l do begin
		read(t[j]);
		readln(b[j]);
	end;

	ans := 0;
	if (rt = ra) and (ct = ca) then inc(ans);

	i := 1;
	j := 1;
	while (i <= m) or (j <= l) do begin

		mn := min(a[i], b[j]);
		case s[i] of

			'U': case t[j] of
				'U': if (rt = ra) and (ct = ca) then inc(ans, mn-1);
				'D': if (rt > ra) and (rt-mn <= ra+mn) then mn = (rt-ra+1) div 2;
				'L': if (rt > ra) and (rt-mn <= ra) and (ct < ca) and (ct >= ca+mn) then mn = max(rt-ra, ca-ct);
				'R': if (rt > ra) and (rt-mn <= ra) and (ct > ca) and (ct <= ca+mn) then mn = max(rt-ra, ct-ca);
			end;

			'D': case t[j] of
				'U': if (rt < ra) and (rt+mn >= ra-mn) then mn = (ra-rt+1) div 2;
				'D': if (rt = ra) and (ct = ca) then inc(ans, mn-1);
				'L': if (rt < ra) and (rt+mn >= ra) and (ct < ca) and (ct >= ca-mn) then mn = max(ra-rt, ca-ct);
				'R': if (rt < ra) and (rt+mn >= ra) and (ct > ca) and (ct <= ca-mn) then mn = max(ra-rt, ct-ca);
			end;

			'L': case t[j] of
				'U': if (rt < ra) and (rt+mn >= ra-mn) then mn = (ra-rt+1) div 2;
				'D': if (rt < ra) and (rt+mn >= ra) and (ct > ca) and (ct <= ca-mn) then mn = max(ra-rt, ct-ca);
				'L': if (rt = ra) and (ct = ca) then inc(ans, mn-1);
				'R': if (rt < ra) and (rt+mn >= ra) and (ct < ca) and (ct >= ca-mn) then mn = max(ra-rt, ca-ct);
			end;

		end;

		case s[i] of
			'U': dec(rt, mn);
			'D': inc(rt, mn);
			'L': dec(ct, mn);
			'R': inc(ct, mn);
		end;

		case t[j] of
			'U': dec(ra, mn);
			'D': inc(ra, mn);
			'L': dec(ca, mn);
			'R': inc(ca, mn);
		end;

		if (rt = ra) and (ct = ca) then inc(ans);
		dec(a[i], mn);
		dec(b[j], mn);
		if a[i] = 0 then inc(i);
		if b[j] = 0 then inc(j);

	end;

end.

```
