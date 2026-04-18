program E_Chords;
uses
	math;
const
    maxn = 200 * 1000;
    max2n = maxn * 2;
var
    n, i, j, ai, bi, t: int32;
    ans: boolean;
    a, b, a2, b2, p, merge: array [1 .. maxn] of int32;
    s: array [1 .. max2n] of int32;
    seen: array [1 .. max2n] of boolean;

procedure msort(l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (
                b[p[j]] <= b[p[k]]
            ) then begin
                merge[i] := p[j];
                inc(j);
            end else begin
                merge[i] := p[k];
                inc(k);
            end;

        for i := l to r-1 do p[i] := merge[i];

    end;
end;

begin
    readln(n);

    for i := 1 to n do begin
        readln(ai, bi);
        a[i] := min(ai, bi);
        b[i] := max(ai, bi);
        p[i] := i;
        seen[i] := false;
        seen[i+n] := false;
    end;

    msort(1, n+1);

    for i := 1 to n do begin
        a2[i] := a[p[i]];
        b2[i] := b[p[i]];
    end;

    for i := 1 to n do begin
        a[i] := a2[i];
        b[i] := b2[i];
    end;

    t := 0;
    i := 1;
    j := 1;
    ans := false;

    while (i <= n) and not ans do begin

        while j < b[i] do begin
            if not seen[j] then begin
                inc(t);
                s[t] := j;
            end;
            inc(j);
        end;

        ans := (t > 0) and (a[i] <> s[t]);
        if not ans then begin
            seen[a[i]] := true;
            seen[b[i]] := true;
            dec(t);
            inc(i);
        end;

    end;

    if ans then
        writeln('Yes')
    else
        writeln('No');
end.
