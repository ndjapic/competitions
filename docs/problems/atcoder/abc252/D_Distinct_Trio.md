# Problem: D_Distinct_Trio.pas

```pascal
program D_Distinct_Trio;
const
    nn = 200 * 1000;
var
    n, i, x: int32;
    ans: int64;
    a, c, l, r, last: array [1 .. nn] of int32;
    d: array [1 .. nn] of int64;

begin
    readln(n);

    for i := 1 to n do read(a[i]); readln;

    for x := 1 to nn do c[x] := 0;

    for i := 1 to n do begin
        l[i] := c[a[i]];
        inc(c[a[i]]);
    end;

    for x := 1 to nn do c[x] := 0;

    for i := n downto 1 do begin
        r[i] := c[a[i]];
        inc(c[a[i]]);
    end;

    ans := 0;
    for i := 1 to n do
        inc(ans, int64(i-1-l[i]) * (n-i-r[i]));

    for x := 1 to nn do begin
        last[x] := 0;
        c[x] := 0;
        d[x] := 0;
    end;

    for i := 1 to n do begin
        x := a[i];
        inc(d[x], int64(i-last[x]-1) * c[x]);
        dec(ans, d[x]);
        last[x] := i;
        inc(c[x]);
    end;

    writeln(ans);
end.

```
