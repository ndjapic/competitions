program B_Round_Robin_Tournament;
uses
	math;
const
    maxn = 100;
var
    n, i, j: int8;
    s: char;
    w, p, cp: array [1 .. maxn] of int8;

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
				(w[p[il]] > w[p[ir]]) or
				(w[p[il]] = w[p[ir]]) and (p[il] < p[ir])
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
	readln(n);

	for i := 1 to n do begin
		w[i] := 0;
		p[i] := i;
	end;

	for i := 1 to n do begin
		for j := 1 to n do begin
			read(s);
			if s = 'o' then
				inc(w[i])
			{else if s = 'x' then
				inc(w[j])};
		end;
		readln;
	end;

	msort(1, n);

	for i := 1 to n-1 do write(p[i], ' ');
	writeln(p[n]);
end.
