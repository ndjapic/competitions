# Задатак: D_Colored_Balls.pas

```pascal
program D_Colored_Balls;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 5000;
    prime = 998244353;
var
    {ntc, tci: int16;}
    n, i, x: int16;
    s, ans: int32;
    m: int64;
    a, c: array [0 .. nn] of int16;
    f: array [0 .. nn*nn] of int32;

procedure modinc(var x: int32; y: int32);
begin
    inc(x, y);
    if x >= prime then dec(x, prime);
end;

begin
    a[0] := 0;
    {readln(ntc);
    for tci := 1 to ntc do} begin

        for x := 1 to nn do c[x] := 0;

        readln(n);
        for i := 1 to n do begin
            read(x);
            inc(c[x]);
        end;
        readln;

        x := 1;
        for i := 1 to n do begin
            while c[x] = 0 do inc(x);
            a[i] := x;
            dec(c[x]);
        end;

        for s := 1 to n*a[n] do f[s] := 0;
        f[0] := 1;

        ans := 0;
        for i := 1 to n do begin
            for s := (i-1)*a[i-1] downto 0 do begin
                if s > a[i] then
                    m := (s + a[i] + 1) div 2
                else
                    m := a[i];
                ans := (m * f[s] + ans) mod prime;
                modinc(f[s + a[i]], f[s]);
            end;
        end;

        writeln(ans);

    end;
end.

```
