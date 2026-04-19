# Problem: C_Sigma.pas

```pascal
program C_Sigma;
const
    maxn = 200 * 1000;
var
    n, k, i: int32;
    ans: int64;
    a, merge: array [1 .. maxn] of int32;

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
            if (k = r) or (j < m) and (a[j] <= a[k]) then begin
                merge[i] := a[j];
                inc(j);
            end else begin
                merge[i] := a[k];
                inc(k);
            end;

        for i := l to r-1 do a[i] := merge[i];

    end;
end;

begin
    readln(n, k);

    for i := 1 to n do read(a[i]);
    readln;
    msort(1, n+1);

    ans := int64(k+1) * k div 2;
    if a[1] <= k then dec(ans, a[1]);

    for i := 2 to n do
        if (a[i] > a[i-1]) and (a[i] <= k) then dec(ans, a[i]);

    writeln(ans);
end.

```
