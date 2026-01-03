program C_Nikita_and_LCM;
const
    nn = 2000;
var
    ntc, tci: int16;
    n, i, j: int16;
    a, b, merge: array [0 .. nn] of int64;

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

function gcd(x, y: int64): int64;
begin
    if y = 0 then
        gcd := x
    else
        gcd := gcd(y, x mod y);
end;

function lcm(x, y: int64): int64;
begin
    if x = 0 then
        lcm := y
    else
        lcm := x div gcd(x, y) * y;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]); readln; msort(1, n+1);

        a[0] := 0;
        b[0] := 0;
        i := 1;

        while (i <= n) and (b[i-1] <= a[n]) do begin
            b[i] := lcm(b[i-1], a[i]);
            inc(i);
        end;

        if (i <= n) or (b[n] > a[n]) then
            writeln(n)
        else begin

            i := n;
            j := n;

            while (i > 0) and (j > 0) and (a[i] = b[j]) do begin
                dec(j);
                if j > 0 then
                    while (i > 0) and (a[i] > b[j]) do dec(i);
            end;

            writeln(j);

        end;

    end;
end.
