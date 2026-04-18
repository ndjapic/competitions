# Задатак: D_Printing_Machine.pas

```pascal
program D_Printing_Machine;
const
    maxn = 200 * 1000 + 1;
    inf = high(int64);
var
    n, i, c: int32;
    t, d, ans: int64;
    e, merge: array [1 .. maxn*2] of record
        t: int64;
        c: int32;
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
            if (ir > r) or (il <= m) and (e[il].t <= e[ir].t) then begin
                merge[i] := e[il];
                inc(il);
            end else begin
                merge[i] := e[ir];
                inc(ir);
            end;

        for i := l to r do e[i] := merge[i];

    end;
end;

begin
    readln(n);

    for i := 1 to n do begin
        readln(t, d);
        e[i].t := t;
        e[i].c := 1;
        e[i+n].t := t+d+1;
        e[i+n].c := -1;
    end;

    msort(1, maxn*2);
    e[2*n+1].t := inf;
    e[2*n+1].c := 0;

    c := 0;
    t := 0;
    ans := 0;

    for i := 1 to maxn*2+1 do begin
        while (c > 0) and (t < e[i].t) do begin
            dec(c);
            inc(t);
            inc(ans);
        end;
        inc(c, e[i].c);
        t := e[i].t;
    end;

    writeln(ans);
end.

```
