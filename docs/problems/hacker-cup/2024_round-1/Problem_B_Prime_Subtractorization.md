# Problem: Problem_B_Prime_Subtractorization.pas

```pascal
program Problem_B_Prime_Subtractorization;
const
    nn = 10 * 1000 * 1000;
var
    ntc, tci: int32;
    n, p: int32;
    isPrime: array [2 .. nn] of boolean;
    c: array [4 .. nn] of int32;

begin
    for p := 2 to nn do isPrime[p] := true;

    p := 2;
    n := p*p;
    while n <= nn do begin
        while n <= nn do begin
            isPrime[n] := false;
            inc(n, p);
        end;
        inc(p);
        n := p*p;
    end;

    c[4] := 0;
    for n := 5 to nn do begin
        c[n] := c[n-1];
        if isPrime[n] and isPrime[n-2] then inc(c[n]);
    end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        write('Case #', tci, ': ');
        if n < 5 then
            writeln(0)
        else
            writeln(c[n] + 1);

    end;
end.

```
