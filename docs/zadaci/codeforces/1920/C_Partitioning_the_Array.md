# Задатак: C_Partitioning_the_Array.pas

```pascal
program C_Partitioning_the_Array;
const
    maxn = 200 * 1000;
var
    ntc, tci, j: int16;
    n, i, k, m, ans: int32;
    a, nd: array [0 .. maxn] of int32;
    d: array [1 .. maxn] of array of int32;

procedure init_d();
var
    n, k: int32;
begin
    for n := 1 to maxn do begin
        setlength(d[n], 1);
        nd[n] := 0;
    end;

    for k := 1 to maxn do begin
        n := k;
        while n <= maxn do begin
            if length(d[n]) = nd[n] then setlength(d[n], 2*nd[n]);
            d[n][nd[n]] := k;
            inc(nd[n]);
            inc(n, k);
        end;
    end;
end;

function gcd(x, y: int32): int32;
begin
    if y = 0 then
        gcd := x
    else
        gcd := gcd(y, x mod y);
end;

begin
    init_d();

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]);
        readln;

        ans := 0;
        for j := 0 to nd[n]-1 do begin
            k := d[n][j];
            m := 0;
            for i := k+1 to n do
                m := gcd(m, abs(a[i] - a[i-k]));
            if m <> 1 then inc(ans);
        end;

        writeln(ans);

    end;
end.

```
