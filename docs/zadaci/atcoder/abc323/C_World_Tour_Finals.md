# Задатак: C_World_Tour_Finals.pas

```pascal
program C_World_Tour_Finals;
uses
	math;
const
    maxn = 100;
var
    n, m, i, j, ans: int8;
    mx: int32;
    s: array [1 .. maxn, 1 .. maxn] of char;
    a, score: array [1 .. maxn] of int32;
    p, cp: array [1 .. maxn] of int8;

procedure msort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m+1, r);

        il := l;
        ir := m+1;
        for i := l to r do
            if (ir > r) or (il <= m) and (
				a[p[il]] >= a[p[ir]]
			) then begin
                cp[i] := p[il];
                inc(il);
            end else begin
                cp[i] := p[ir];
                inc(ir);
            end;

        for i := l to r do p[i] := cp[i];

    end;
end;

begin
	readln(n, m);

	for j := 1 to m do begin
		read(a[j]);
		p[j] := j;
	end;
	readln;

	msort(1, m);

	mx := 0;
	for i := 1 to n do begin

		score[i] := i;
		for j := 1 to m do begin
			read(s[i, j]);
			if s[i, j] = 'o' then
				inc(score[i], a[j]);
		end;
		readln;
		mx := max(mx, score[i]);

	end;

	for i := 1 to n do begin

		ans := 0;
		for j := 1 to m do begin
			if s[i, p[j]] = 'x' then begin
				if score[i] < mx then inc(ans);
				inc(score[i], a[p[j]]);
			end;
		end;
		writeln(ans);

	end;

end.

```
