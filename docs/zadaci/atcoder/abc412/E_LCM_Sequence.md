# Задатак: E_LCM_Sequence.pas

```pascal
program E_LCM_Sequence;
uses
    math;
const
    nn = 10 * 1000 * 1000;
var
    l, r, n, i, p, pp: int64;
    ans: int32;
    isPrime, isPrimePower: array [0 .. nn] of boolean;

begin
    readln(l, r);

    for p := 1 to nn do isPrime[p] := true;
    isPrime[1] := false;

    for n := l to r do isPrimePower[n-l] := true;
    if l = 1 then isPrimePower[0] := false;

    p := 2;
    pp := p*p;
    while pp <= nn do begin
        if isPrime[p] then begin

            while pp <= nn do begin
                isPrime[pp] := false;
                inc(pp, p);
            end;

        end;
        inc(p);
        pp := p*p;
    end;

    for p := 2 to nn do
        if isPrime[p] then begin

            for i := max(p, (l+p-1) div p) to r div p do
                isPrimePower[i*p-l] := false;

            n := 1;
            while n <= r div p do begin
                n := n*p;
                if n >= l then isPrimePower[n-l] := true;
            end;

        end;

    ans := 1;
    for n := l+1 to r do
        if isPrimePower[n-l] then inc(ans);
    writeln(ans);
end.

```
