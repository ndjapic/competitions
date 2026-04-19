# Problem: C_Transportation_Expenses.pas

```pascal
program C_Transportation_Expenses;
{$mode objfpc}{$H+}{$J-}
const
    nn = 200 * 1000;
var
    n, i: int32;
    m: int64;
    a, merge: array [0 .. nn] of int64;

procedure msort(lend, rend: int32);
var
    i, l, r, m: int32;

    function LessOrEqual(l, r: int32): boolean;
    begin
        result := a[l] <= a[r];
    end;

begin
    i := lend + 1;
    while (i < rend) and LessOrEqual(i-1, i) do inc(i);

    if i < rend then begin

        m := (lend + rend) div 2;
        if i < m then msort(lend, m);
        msort(m, rend);

        l := lend;
        r := m;
        for i := lend to rend - 1 do
            if (r = rend) or (l < m) and LessOrEqual(l, r) then begin
                merge[i] := a[l];
                inc(l);
            end else begin
                merge[i] := a[r];
                inc(r);
            end;

        for i := lend to rend - 1 do a[i] := merge[i];

    end;
end;

begin
    readln(n, m);

    for i := 1 to n do read(a[i]);
    readln;
    msort(1, n+1);

    a[0] := 0;
    for i := 1 to n do inc(a[i], a[i-1]);

    i := 0;
    while (i <= n) and (a[i] + (a[i] - a[i-1]) * (n-i) <= m) do inc(i);
    dec(i);

    if i = n then
        writeln('infinite')
    else
        writeln((m-a[i]) div (n-i));
end.

```
