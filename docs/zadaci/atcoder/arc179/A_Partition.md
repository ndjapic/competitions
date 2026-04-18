# Задатак: A_Partition.pas

```pascal
program A_Partition;
const
    nn = 200 * 1000;
var
    n, k, i: int32;
    a, merge: array [1 .. nn] of int32;
    s: array [0 .. nn] of int64;

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
            if (k = r) or (j < m) and (
                a[j] <= a[k]
            ) then begin
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

    s[0] := 0;
    if k <= 0 then begin

        for i := 1 to n do s[i] := s[i-1] + a[n+1-i];

        if s[n] < k then
            writeln('No')
        else begin
            writeln('Yes');
            for i := n downto 2 do write(a[i], ' ');
            writeln(a[1]);
        end;

    end else begin

        for i := 1 to n do s[i] := s[i-1] + a[i];

        writeln('Yes');
        for i := 1 to n-1 do write(a[i], ' ');
        writeln(a[n]);

    end;
end.

```
