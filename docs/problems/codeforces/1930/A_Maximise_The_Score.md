# Problem: A_Maximise_The_Score.pas

```pascal
program A_Maximise_The_Score;
const
    max2n = 100;
var
    ntc, tci: int16;
    n, i: int8;
    ans: int32;
    a, merge: array [1 .. max2n] of int32;

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
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n);

        for i := 1 to 2*n do read(a[i]);
        readln;
        msort(1, 2*n+1);

        ans := 0;
        for i := 1 to n do
            inc(ans, a[2*i-1]);

        writeln(ans);
    end;
end.

```
