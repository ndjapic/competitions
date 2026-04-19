# Problem: sieve.pas

```pascal
program sieve;
uses
    math;
const
    nn = 1000 * 1000;
var
    n, p, nop: int32;
    isPrime: array [2 .. nn] of boolean;
    primes: array [1 .. nn] of int32;

function isqrt(a: int64): int64;
var
    l, r: int64;
begin
    r := min(a, high(int32));
    l := a div r;
    while r-l > 1 do begin
        r := (l+r) div 2;
        l := a div r;
    end;
    isqrt := l;
end;

begin
    for p := 2 to nn do isPrime[p] := true;

    for p := 2 to isqrt(nn) do
        if isPrime[p] then begin
            n := p*p;
            while n <= nn do begin
                isPrime[n] := false;
                inc(n, p);
            end;
        end;

    nop := 0; (* Number of primes *)
    for p := 2 to nn do
        if isPrime[p] then begin
            inc(nop);
            primes[nop] := p;
            write(' ', p); (* REMOVE THIS LINE! *)
        end;
    writeln; (* REMOVE THIS LINE! *)
end.

```
