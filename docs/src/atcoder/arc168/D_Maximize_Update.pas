program D_Maximize_Update;
uses
    math;
const
    maxn = 500;
    maxm = 250 * 501;
type
    telm = record
        l, r, d: int16;
    end;
var
    n, i, l, r: int16;
    m, j, j0, k, ans, ansl, ansr: int32;
    t: telm;
    seenl, seenr: array [1 .. maxn] of boolean;
    changed: boolean;
    s: array [1 .. maxn] of char;
    a, merge: array [1 .. maxm] of telm;

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
                (a[il].d < a[ir].d) or
                (a[il].d = a[ir].d) and (a[il].l > a[ir].l)
            ) then begin
                merge[i] := a[il];
                inc(il);
            end else begin
                merge[i] := a[ir];
                inc(ir);
            end;

        for i := l to r do a[i] := merge[i];

    end;
end;

begin
    readln(n, m);

    for i := 1 to n do begin
        s[i] := 'W';
        seenl[i] := false;
        seenr[i] := false;
    end;

    for j := 1 to m do begin
        readln(l, r);
        a[j].l := l;
        a[j].r := r;
        a[j].d := r-l+1;
        seenl[l] := true;
        seenr[r] := true;
    end;

    ans := 0;
    for j := 1 to m do begin

        j0 := j;
        for k := j to m do
            if a[k].r begin
                a[j].d := 0;
                for i := a[k].l to a[k].r do
                    if s[i] = 'W' then inc(a[k].d);
                if a[k].d < a[j0].d then j0 := k;
            end;

        t := a[j0];
        a[j0] := a[j];
        a[j] := t;

        changed := false;
        for i := a[j].l to a[j].r do
            if s[i] = 'W' then begin
                changed := true;
                s[i] := 'B';
            end;

        if changed then inc(ans);

    end;

    ansl := 0;
    for l := 1 to n do
        if seenl[l] then inc(ansl);
    ans := max(ans, ansl);

    ansr := 0;
    for r := 1 to n do
        if seenr[r] then inc(ansr);
    ans := max(ans, ansr);

    writeln(ans);
end.
