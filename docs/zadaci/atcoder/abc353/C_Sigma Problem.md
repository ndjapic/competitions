# Задатак: C_Sigma Problem.pas

```pascal
program C_Sigma_Problem;
const
    nn = 300 * 1000;
    ten8 = 100 * 1000 * 1000;
var
    n, i, j: int32;
    s, c: qword;
    a, merge: array [1 .. nn] of int32;

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
    readln(n);

    s := 0;
    c := 0;
    for i := 1 to n do begin
        read(a[i]);
        inc(s, a[i] * 2);
        if a[i] * 2 >= ten8 then dec(c);
    end;
    readln;

    msort(1, n+1);

    j := n;
    for i := 1 to n do begin
        while (j > 0) and (a[i] + a[j] >= ten8) do dec(j);
        inc(c, n-j);
    end;

    writeln((s * (n-1) - c * ten8) div 2);
end.

```
