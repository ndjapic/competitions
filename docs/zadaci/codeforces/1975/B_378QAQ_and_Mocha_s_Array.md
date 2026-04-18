# Задатак: B_378QAQ_and_Mocha_s_Array.pas

```pascal
program B_378QAQ_and_Mocha_s_Array;
const
    nn = 300 * 1000;
var
    ntc, tci: int16;
    n, i, j: int32;
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
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(a[i]);
        readln;
        msort(1, n+1);

        j := 2;
        while (j <= n) and (a[j] mod a[1] = 0) do inc(j);
        i := j+1;
        while (i <= n) and ((a[i] mod a[1] = 0) or (a[i] mod a[j] = 0)) do inc(i);

        if i <= n then
            writeln('No')
        else
            writeln('Yes');

    end;
end.

```
