# Задатак: C_Everything_Nim.pas

```pascal
program C_Everything_Nim;
uses
    math;
const
    sz = 200 * 1000;
var
    ntc, tci: int16;
    n, m, i{, l ,r}: int32;
    a, b: array [0 .. sz] of int32;
    win: array [0 .. sz] of boolean;

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
                b[i] := a[j];
                inc(j);
            end else begin
                b[i] := a[k];
                inc(k);
            end;

        for i := l to r-1 do a[i] := b[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(a[i]);
        readln;
        msort(1, n+1);

        b[0] := 0;
        b[1] := a[1];
        m := 1;

        for i := 2 to n do
            if a[i-1] < a[i] then begin
                inc(m);
                b[m] := a[i];
            end;

        {l := 0;
        while (l < m) and (a[l+1] - a[l] = 1) do inc(l);}

        win[m] := false;
        for i := m-1 downto 0 do
            if b[i+1] - b[i] = 1 then
                win[i] := not win[i+1]
            else
                win[i] := true;

        if win[0] then
            writeln('Alice')
        else
            writeln('Bob');

        {i := 1;
        while (i <= m) and (b[i] - b[i-1] = 1) do dec(i);

        if i <= m then begin

            if odd(i) then
                writeln('Alice')
            else
                writeln('Bob');

        end else begin

            if odd(m) then
                writeln('Alice')
            else
                writeln('Bob');

        end;}

    end;
end.

```
