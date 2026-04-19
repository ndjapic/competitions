# Problem: tile_permutation.pas

```pascal
program tile_permutation;
const
    maxn = 10 * 1000;
    prime = 1000 * 1000 * 1000 + 7;
var
    q: int8;
    n, x, dw, dow, i, r: int32;
    fact, invf: array [0 .. maxn] of int32;
 
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
    ncr := int64(invf[r]) * invf[n-r] mod prime * fact[n] mod prime
end;
 
begin
    fact[0] := 1;
    for n := 1 to maxn do fact[n] := int64(n) * fact[n-1] mod prime;
    invf[maxn] := modpow(fact[maxn], prime - 2);
    for n := maxn downto 1 do invf[n-1] := int64(n) * invf[n] mod prime;
 
    readln(q);
    repeat
 
        readln(n, x);
        dow := n div x + 1;
        dw := 0;
 
        for i := 0 to dow-1 do begin
            r := n - i*x;
            inc(dw, ncr(r+i, r));
            if dw >= prime then dec(dw, prime);
        end;
 
        dec(dw, dow);
        if dw < 0 then inc(dw, prime);
        writeln(dw);
 
        dec(q);
    until q = 0;
end.

```
