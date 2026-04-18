# Задатак: E_Romantic_Glasses.pas

```pascal
program E_Romantic_Glasses;
const
    maxn = 200 * 1000;
    inf = 1000 * 1000 * 1000 * 1000 * 1000;
var
    ntc, tci: int16;
    n, i: int32;
    a: array [1 .. maxn] of int32;
    s, merge: array [0 .. maxn] of int64;

procedure msort(l, r: int32);
var
    m, i, j, k: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m+1, r);

        j := l;
        k := m+1;
        for i := l to r do
            if (k > r) or (j <= m) and (s[j] <= s[k]) then begin
                merge[i] := s[j];
                inc(j);
            end else begin
                merge[i] := s[k];
                inc(k);
            end;

        for i := l to r do s[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        s[0] := 0;

        for i := 1 to n do begin
            read(a[i]);
            if odd(i) then
                s[i] := s[i-1] + a[i]
            else
                s[i] := s[i-1] - a[i];
        end;
        readln;

        msort(0, n);

        i := 1;
        while (i <= n) and (s[i-1] < s[i]) do inc(i);

        if i <= n then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
