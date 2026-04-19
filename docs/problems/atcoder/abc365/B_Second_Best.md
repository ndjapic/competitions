# Problem: B_Second_Best.pas

```pascal
program B_Second_Best;
{$mode objfpc}{$H+}{$J-}
const
    nn = 100;
var
    n, i: int8;
    a, p, merge: array [1 .. nn] of int32;

procedure msort(lend, rend: int32);
var
    i, l, r, m: int32;

    function LessOrEqual(l, r: int32): boolean;
    begin
        result := a[p[l]] >= a[p[r]];
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
                merge[i] := p[l];
                inc(l);
            end else begin
                merge[i] := p[r];
                inc(r);
            end;

        for i := lend to rend - 1 do p[i] := merge[i];

    end;
end;

begin
    readln(n);

    for i := 1 to n do begin
        read(a[i]);
        p[i] := i;
    end;
    readln;
    msort(1, n+1);

    writeln(p[2]);
end.

```
