# Задатак: D_String_Bags.pas

```pascal
program D_String_Bags;
const
    maxn = 100;
    maxs = maxn * maxn * maxn;
var
    n, m, l, q, i, t, u, v, w, x: int32;
    a, b, c: array [1 .. maxn] of int32;
    s, merge: array [1 .. maxs] of int32;

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
                s[j] <= s[k]
            ) then begin
                merge[i] := s[j];
                inc(j);
            end else begin
                merge[i] := s[k];
                inc(k);
            end;

        for i := l to r-1 do s[i] := merge[i];

    end;
end;

function bisectr(x: int32): int32;
var
    l, r, m: int32;
begin
    l := 0;
    r := t+1;
    while r-l > 1 do begin
        m := (l+r) div 2;
        if x < s[m] then
            r := m
        else
            l := m;
    end;
    bisectr := r;
end;

function numelm(x: int32): int32;
begin
    numelm := bisectr(x) - bisectr(x-1);
end;

begin
    readln(n); for u := 1 to n do read(a[u]); readln;
    readln(m); for v := 1 to m do read(b[v]); readln;
    readln(l); for w := 1 to l do read(c[w]); readln;

    t := 0;
    for u := 1 to n do
        for v := 1 to m do
            for w := 1 to l do begin
                inc(t);
                s[t] := a[u] + b[v] + c[w];
            end;

    msort(1, t+1);

    readln(q);
    for i := 1 to q do begin
        read(x);
        if numelm(x) > 0 then
            writeln('Yes')
        else
            writeln('No');
    end;
    readln;
end.

```
