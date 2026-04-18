# Задатак: C_Good_Prefixes.pas

```pascal
program C_Good_Prefixes;
{$mode objfpc}{$H+}{$J-}
const
    nn = 200 * 1000 + 1;
var
    ntc, tci: int16;
    n, i, j, x, ans: int32;
    s: int64;
    randtime: array [1 .. nn] of int32;
    th: record
        a: array [0 .. nn] of record
            x, y, l, r, n: int32;
        end;
        n, root: int32;
    end;

procedure th_init();
var
    i, j: int32;
begin
    randomize;
    for i := 1 to n do begin
        j := 1 + random(i);
        randtime[i] := randtime[j];
        randtime[j] := i;
    end;
    th.root := 0;
    th.n := 0;
    th.a[0].x := 0;
    th.a[0].y := high(int32);
    th.a[0].l := 0;
    th.a[0].r := 0;
    th.a[0].n := 0;
end;

function th_append(x, y: int32): int32;
begin
    inc(th.n);
    th.a[th.n].x := x;
    th.a[th.n].y := y;
    th.a[th.n].l := 0;
    th.a[th.n].r := 0;
    th.a[th.n].n := 1;
    th_append := th.n;
end;

procedure th_update(u: int32);
begin
    th.a[u].n := th.a[th.a[u].l].n + th.a[th.a[u].r].n + 1;
end;

function th_rotate(u, v: int32): int32;
begin
    {if v > 0 then} begin
        if th.a[u].l = v then begin
            th.a[u].l := th.a[v].r;
            th.a[v].r := u;
        end else begin
            th.a[u].r := th.a[v].l;
            th.a[v].l := u;
        end;
        th_update(u);
        th_update(v);
        u := v;
    end;
    th_rotate := u;
end;

function th_insert(u, x, y: int32): int32;
begin
    if u = 0 then begin
        u := th_append(x, y);
    end else if x <= th.a[u].x then begin
        th.a[u].l := th_insert(th.a[u].l, x, y);
        if y > th.a[u].y then u := th_rotate(u, th.a[u].l);
    end else begin
        th.a[u].r := th_insert(th.a[u].r, x, y);
        if y > th.a[u].y then u := th_rotate(u, th.a[u].r);
    end;
    th_update(u);
    th_insert := u;
end;

function th_get(u, i: int32): int32;
var
    v, j: int32;
begin
    v := th.a[u].l;
    j := i - th.a[v].n - 1;
    if j < 0 then
        th_get := th_get(v, i)
    else if j > 0 then
        th_get := th_get(th.a[u].r, j)
    else
        th_get := th.a[u].x;
end;

function th_ord(u, x: int32): int32;
var
    v: int32;
begin
    if u = 0 then
        th_ord := 0
    else if x < th.a[u].x then begin
        v := th.a[u].l;
        th_ord := th_ord(v, x);
    end else begin
        v := th.a[u].r;
        th_ord := th.a[u].n - th.a[v].n + th_ord(v, x);
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        th_init();

        ans := 0;
        s := 0;
        for i := 1 to n do begin
            read(x);
            th.root := th_insert(th.root, 2*x, randtime[i]);
            inc(s, x);
            j := th_ord(th.root, s);
            if (j > 0) and (th_get(th.root, j) = s) then inc(ans);
        end;

        writeln(ans);

    end;
end.

```
