program C_Medium_Design;
uses
    math;
const
    maxn = 100 * 1000;
var
    ntc, tci, n, m, i, ne, cost, ans: int32;
    a: array [1 .. maxn] of record
        l, r: int32;
    end;
    evt, merge: array [1 .. maxn*2] of record
        j: int32;
        c: int8;
    end;

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
            if (ir > r) or (il <= m) and (evt[il].j <= evt[ir].j) then begin
                merge[i] := evt[il];
                inc(il);
            end else begin
                merge[i] := evt[ir];
                inc(ir);
            end;

        for i := l to r do evt[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

        for i := 1 to n do readln(a[i].l, a[i].r);

        ans := 0;

        ne := 0;
        for i := 1 to n do
            if a[i].r < m then begin
                inc(ne);
                evt[ne].j := a[i].l;
                evt[ne].c := 1;
                inc(ne);
                evt[ne].j := a[i].r + 1;
                evt[ne].c := -1;
            end;

        msort(1, ne);
        cost := 0;

        for i := 1 to ne do begin
            inc(cost, evt[i].c);
            if (i = ne) or (evt[i].j < evt[i+1].j) then ans := max(ans, cost);
        end;

        ne := 0;
        for i := 1 to n do
            if a[i].l > 1 then begin
                inc(ne);
                evt[ne].j := a[i].l;
                evt[ne].c := 1;
                inc(ne);
                evt[ne].j := a[i].r + 1;
                evt[ne].c := -1;
            end;

        msort(1, ne);
        cost := 0;

        for i := 1 to ne do begin
            inc(cost, evt[i].c);
            if (i = ne) or (evt[i].j < evt[i+1].j) then ans := max(ans, cost);
        end;

        writeln(ans);

    end;
end.
