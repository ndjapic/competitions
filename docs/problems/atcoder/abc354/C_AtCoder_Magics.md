# Problem: C_AtCoder_Magics.pas

```pascal
program C_AtCoder_Magics;
const
    nn = 200 * 1000;
var
    n, m, i: int32;
    a, c, p, merge: array [1 .. nn] of int32;
    s: array [1 .. nn] of record
        i, a, c: int32;
    end;
    active: array [1 .. nn] of boolean;

procedure msorti(l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msorti(l, m);
        msorti(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (
                a[p[j]] < a[p[k]]
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

procedure push(i: int32);
begin
    inc(m);
    s[m].i := p[i];
    s[m].a := a[p[i]];
    s[m].c := c[p[i]];
end;

begin
    readln(n);

    for i := 1 to n do begin
        readln(a[i], c[i]);
        p[i] := i;
        active[i] := false;
    end;

    msorti(1, n+1);

    m := 0;
    push(1);

    for i := 2 to n do begin
        while (m > 0) and (s[m].c > c[p[i]]) do dec(m);
        push(i);
    end;

    writeln(m);
    for i := 1 to m do active[s[i].i] := true;
    for i := 1 to n do
        if active[i] then write(i, ' ');
    writeln;
end.

```
