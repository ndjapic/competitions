# Problem: D_Even_String.pas

```pascal
program D_Even_String;
{$INLINE ON}
const
    nn = 500 * 1000;
    prime = 998244353;
var
    ntc, tci: int16;
    n, h, p, s, mask, ans: int32;
    e: int8;
    cond: boolean;
    c: array [0 .. 25] of int32;
    fact, invf: array [0 .. nn] of int32;

function modmul(a, b: int32): int32; inline;
begin
    modmul := int64(a) * b mod prime;
end;

function modpow(b, e: int32): int32;
begin
    if e = 0 then
        modpow := 1
    else if odd(e) then
        modpow := modmul(b, modpow(b, e-1))
    else
        modpow := modpow(modmul(b, b), e div 2);
end;

begin
    fact[0] := 1;
    for n := 1 to nn do fact[n] := modmul(n, fact[n-1]);
    invf[nn] := modpow(fact[nn], prime - 2);
    for n := nn downto 1 do invf[n-1] := modmul(n, invf[n]);

    readln(ntc);
    for tci := 1 to ntc do begin

        n := 0;
        for e := 0 to 25 do begin
            read(c[e]);
            inc(n, c[e]);
        end;
        readln;
        h := n div 2;

        p := modmul(fact[h], fact[n-h]);
        for e := 0 to 25 do p := modmul(p, invf[c[e]]);

        ans := 0;
        for mask := 0 to (1 shl 26) - 1 do begin
            s := 0;
            cond := true;
            for e := 0 to 25 do
                if cond and odd(mask shr e) then begin
                    inc(s, c[e]);
                    if c[e] = 0 then cond := false;
                end;
            if (s = h) and cond then inc(ans);
        end;

        writeln(modmul(ans, p));

    end;
end.

```
