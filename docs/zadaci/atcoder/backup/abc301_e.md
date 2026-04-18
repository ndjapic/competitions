# Задатак: abc301_e.pas

```pascal
program abc301_e;
uses
    math;
const
    maxk = 300 * 300;
    maxm = 1024 * 1024;
    inf = 2000 * 1000 + 1;
var
    h, w, i, j: int16;
    t, k, k2, s, g, l, r, m, m2, maxm2: int32;
    nc, ci, cj, e, ans: int8;
    c: array [0 .. 19] of int32;
    cd: array [0 .. 19, 0 .. 19] of int32;
    md: array [0 .. maxm, 0 .. 19] of int32;
    nosb: array [0 .. maxm] of int8;
    v: array [1 .. maxk] of record
        a: char;
        d: int32;
        adj: array of int32;
    end;
    q: array [1 .. maxk] of int32;

procedure arrow(k1, k2: int32);
var
    e: int8;
begin
    if (v[k1].a <> '#') and (v[k2].a <> '#') then begin
        e := length(v[k1].adj);
        setlength(v[k1].adj, e+1);
        v[k1].adj[e] := k2;
    end;
end;

begin
    readln(h, w, t);
    k := 0;
    nc := 0;

    for i := 1 to h do begin
        for j := 1 to w do begin

            inc(k);
            read(v[k].a);
            setlength(v[k].adj, 0);

            if i > 1 then begin
                arrow(k, k-w);
                arrow(k-w, k);
            end;

            if j > 1 then begin
                arrow(k, k-1);
                arrow(k-1, k);
            end;

            case v[k].a of
                'S': s := k;
                'G': g := k;
                'o': begin
                    inc(nc);
                    c[nc] := k;
                end;
            end;

        end;
        readln;
    end;

    inc(nc);
    c[nc] := g;
    c[0] := s;

    for ci := 0 to nc do begin

        for k := 1 to int32(h)*w do v[k].d := inf;
        l := 1;
        r := 1;
        k := c[ci];
        v[k].d := 0;
        q[1] := k;

        while l <= r do begin

            k := q[l];
            inc(l);

            for e := 0 to length(v[k].adj) - 1 do begin
                k2 := v[k].adj[e];
                if v[k2].d = inf then begin
                    inc(r);
                    q[r] := k2;
                    v[k2].d := v[k].d + 1;
                end;
            end;

        end;

        for cj := 0 to nc do cd[ci, cj] := v[c[cj]].d;

    end;

    nosb[0] := 0;
    maxm2 := (int32(1) shl (nc+1)) - 1;
    for m := 0 to maxm2 do begin
        nosb[m] := nosb[m div 2] + m mod 2;
        for ci := 0 to nc do md[m, ci] := inf;
    end;
    md[1, 0] := 0;

    for m := 0 to maxm2 do
        for ci := 0 to nc-1 do
            if md[m, ci] < t then
                for cj := 1 to nc do
                    if not odd(m shr cj) then begin
                        m2 := m + (int32(1) shl cj);
                        md[m2, cj] := min(
                            md[m2, cj], md[m, ci] + cd[ci, cj]
                        );
                    end;

    ans := 1;
    for m := (maxm2+1) div 2 to maxm2 do
        if md[m, nc] <= t then ans := max(ans, nosb[m]);

    writeln(ans-2);
end.


```
