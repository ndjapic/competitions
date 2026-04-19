# Problem: D_Divide_Interval.pas

```pascal
program D_Divide_Interval;
var
    r, j: int64;
    m, i: int8;
    l: array of int64;

begin
    setlength(l, 2);
    m := 1;
    readln(l[m], r);

    i := 60;
    while l[m] < r do begin

        while (l[m] shr i) shl i < l[m] do dec(i);
        j := l[m] shr i;

        while (j+1) shl i > r do begin
            dec(i);
            inc(j, j);
        end;

        inc(m);
        if length(l) = m then setlength(l, 2*m);
        l[m] := (j+1) shl i;
        while (l[m] shr i) shl i = l[m] do inc(i);

    end;

    dec(m);
    writeln(m);
    for i := 1 to m do writeln(l[i], ' ', l[i+1]);
end.

```
