program Sum_of_Goodness;
const
    maxn = 100 * 1000;
    prime = 1000 * 1000 * 1000 + 7;
var
    notc: int32;
    n, i, ai, ans: int32;
    a, c: array [1 .. maxn] of int32;
    pow2, fact, invf: array [0 .. maxn] of int32;

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
    ncr := int64(fact[n]) * invf[r] mod prime * invf[n-r] mod prime;
end;

begin
    pow2[0] := 1;
    for n := 1 to maxn do begin
        pow2[n] := pow2[n-1] * 2;
        if pow2[n] >= prime then dec(pow2[n], prime);
    end;

    fact[0] := 1;
    for n := 1 to maxn do
        fact[n] := int64(n) * fact[n-1] mod prime;

    invf[maxn] := modpow(fact[maxn], prime - 2);
    for n := maxn downto 1 do
        invf[n-1] := int64(n) * invf[n] mod prime;

    readln(notc);
    repeat

        readln(n);

        for ai := 1 to n do c[ai] := 0;

        for i := 1 to n do begin
            read(ai);
            inc(c[ai]);
        end;
        readln;

        ai := 1;
        for i := 1 to n do begin
            while c[ai] = 0 do inc(ai);
            a[i] := ai;
            dec(c[ai]);
        end;

        ans := 0;
        for i := 1 to n do
            if a[i] <= i then
                ans := (
                    ans + int64(pow2[n-i]) * ncr(i-1, a[i]-1)
                ) mod prime;

        writeln(ans);

        dec(notc);
    until notc = 0;
end.

