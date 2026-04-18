# Задатак: E_Alphabet_Tiles.pas

```pascal
program E_Alphabet_Tiles;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    kk = 1000;
    prime = 998244353;
var
    k, i, j: int16;
    ans: int32;
    c: array [1 .. 26] of int16;
    fact, invf: array [0 .. kk] of int32;

function modpow(b, e: int32): int32;
begin
    if e = 0 then
        modpow := 1
    else if odd(e) then
        modpow := int64(b) * modpow(b, e-1) mod prime
    else
        modpow := modpow(int64(b) * b mod prime, e div 2);
end;

function ncr(n, r: int32): int32;
begin
    ncr := int64(invf[r]) * invf[n-r] mod prime * fact[n] mod prime;
end;

procedure modinc(var a: int32; b: int32);
begin
    inc(a, b);
    if a >= prime then dec(a, prime);
end;

procedure moddec(var a: int32; b: int32);
begin
    dec(a, b);
    if a < 0 then inc(a, prime);
end;

begin
    fact[0] := 1;
    for k := 1 to kk do fact[k] := int64(k) * fact[k-1] mod prime;
    invf[kk] := modpow(fact[kk], prime - 2);
    for k := kk downto 1 do invf[k-1] := int64(k) * invf[k] mod prime;

    readln(n, m);

    pq.n := 0;
    for i := 1 to n do begin
        read(a[i]);
        pqins(1+pq.n, a[i]);
    end;
    readln;

    for i := 1 to n do begin
        a[i] := pq.a[1];
        pqdel(1);
    end;

    pq.n := 0;
    for j := 1 to m do begin
        read(b[j]);
        pqins(1+pq.n, b[j]);
    end;
    readln;

    for j := 1 to m do begin
        b[j] := pq.a[1];
        pqdel(1);
    end;

    i := 1;
    j := 1;
    ans := 0;
    while (i <= n) and (j <= m) do begin
        if a[i] >= b[j] then begin
            inc(ans, a[i]);
            inc(j);
        end;
        inc(i);
    end;

    if j <= m then ans := -1;
    writeln(ans);
end.

```
