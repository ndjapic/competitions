# Problem: E_Interesting_Ratio.pas

```pascal
program E_Interesting_Ratio;
const
    nn = 10 * 1000 * 1000;
    pc = 664 * 1000 + 579;
var
    ntc, tci: int16;
    n, p, i: int32;
    ans: int64;
    sieve: array [2 .. nn] of boolean;
    primes: array [1 .. pc] of int32;

begin
    for p := 2 to nn do sieve[p] := true;

    p := 2;
    n := p*p;
    while n <= nn do begin
        if sieve[p] then
            while n <= nn do begin
                sieve[n] := false;
                inc(n, p);
            end;
        inc(p);
        n := p*p;
    end;

    i := 0;
    for p := 2 to nn do
        if sieve[p] then begin
            inc(i);
            primes[i] := p;
        end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        ans := 0;
        i := 1;
        p := 2;

        while (i <= pc) and (p <= n) do begin
            if sieve[p] then inc(ans, n div p);
            inc(i);
            p := primes[i];
        end;

        writeln(ans);

    end;
end.

```
