# Задатак: D_9_Divisors.pas

```pascal
program D_9_Divisors;
{$mode delphi}
uses
    math;
const
    nn = 1000 * 1000;
var
    n: int64;
    p, nop, i, j: int32;
    isPrime: array [2 .. nn] of boolean;
    primes: array [1 .. nn] of int32;
    c: array [1 .. 2*nn] of int32;

function isqrt(a: int64): int64;
var
    x: int64;
begin
    x := min(a, high(int32));
    while x * x > a do
        x := (x + a div x) div 2;
    isqrt := x;
end;

begin
    for p := 2 to nn do isPrime[p] := true;

    p := 2;
    n := p*p;
    while n <= nn do begin
        if isPrime[p] then begin
            while n <= nn do begin
                isPrime[n] := false;
                inc(n, p);
            end;
        end;
        inc(p);
        n := p*p;
    end;

    nop := 0;
    for p := 2 to nn do
        if isPrime[p] then begin
            inc(nop);
            primes[nop] := p;
        end;

    for n := 1 to 2*nn do c[n] := 0;

    for i := 2 to nop do begin
        p := primes[i];
        j := 1;
        n := primes[j] * p;
        while (j < i) and (n <= 2*nn) do begin
            c[n] := 1;
            inc(j);
            n := primes[j] * p;
        end;
    end;

    i := 1;
    p := primes[i];
    n := sqr(sqr(p));
    while n <= 2*nn do begin
        c[n] := 1;
        inc(i);
        p := primes[i];
        n := sqr(sqr(p));
    end;

    for n := 2 to 2*nn do inc(c[n], c[n-1]);

    readln(n);
    writeln(c[isqrt(n)]);
end.


```
