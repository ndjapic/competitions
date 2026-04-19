# Problem: C_Ideal_Holidays.pas

```pascal
program C_Ideal_Holidays;
uses
    math;
const
    maxn = 200 * 1000;
var
    n, i: int32;
    a, b, d, mx: int64;
    arr, merge: array [0 .. maxn] of int64;

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
            if (k = r) or (j < m) and (arr[j] <= arr[k]) then begin
                merge[i] := arr[j];
                inc(j);
            end else begin
                merge[i] := arr[k];
                inc(k);
            end;

        for i := l to r-1 do arr[i] := merge[i];

    end;
end;

begin
    readln(n, a, b);

    for i := 0 to n-1 do begin
        read(d);
        arr[i] := (d-1) mod (a+b);
    end;
    readln;

    msort(0, n);
    arr[n] := arr[0] + a+b;

    mx := 0;
    for i := 1 to n do
        mx := max(mx, arr[i] - arr[i-1]);

    if mx >= b+1 then
        writeln('Yes')
    else
        writeln('No');
end.

```
