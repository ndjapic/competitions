# Problem: zbir_prostih_prost.pas

```pascal
program zbir_prostih_prost;
const
    maxn = 1000 * 1000;
var
    n, i, p, q, len, ans: int32;
    is_prime: array [2 .. maxn] of boolean;
    primes: array of int32;

begin
    for p := 2 to maxn do is_prime[p] := true;

    for p := 2 to 1000 do
        if is_prime[p] then begin
            n := p*p;
            while n <= maxn do begin
                is_prime[n] := false;
                inc(n, p);
            end;
        end;

    len := 0;
    setlength(primes, 1);
    for p := 2 to maxn do
        if is_prime[p] then begin
            if len = length(primes) then setlength(primes, 2*len);
            primes[len] := p;
            inc(len);
        end;

    readln(n);
    i := 1;
    q := primes[i];
    ans := 0;
    while (i < len) and (2+q <= n) do begin
        if is_prime[2+q] then inc(ans);
        inc(i);
        if i < len then q := primes[i];
    end;

    writeln(ans);
end.

```
