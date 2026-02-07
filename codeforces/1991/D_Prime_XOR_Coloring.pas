program D_Prime_XOR_Coloring;
uses
    math;
const
    nn = 256 * 1025;
var
    ntc, tci: int16;
    n, p, nop, i, j, k, u, v, p2e: int32;
    e: int8;
    c: array [1 .. nn] of int32;
    isPrime, seen: array [0 .. nn] of boolean;
    primes: array [1 .. nn] of int32;

begin
    for n := 2 to nn do isPrime[n] := true;
    isPrime[0] := false;
    isPrime[1] := false;

    j := 0;
    for p := 2 to nn do
        if isPrime[p] then begin
            inc(j);
            primes[j] := p;
            for i := p to nn div p do
                isPrime[i*p] := false;
        end;
    nop := j;

    n := nn;

    e := 0;
    p2e := 1;
    k := 0;

    for u := 1 to n do begin

        while u shr e > 0 do begin
            inc(e);
            inc(p2e, p2e);
        end;

        for i := 1 to k do seen[i] := false;
        c[u] := k;

        j := 1;
        while (j <= nop) and (primes[j] < p2e) do begin
            v := u xor primes[j];
            if (0 < v) and (v < u) then seen[c[v]] := true;
            inc(j);
        end;

        while seen[c[u]] do dec(c[u]);
        if c[u] = 0 then begin
            inc(k);
            c[u] := k;
        end;

    end;
    writeln(k);

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        k := 1;
        for u := 1 to n do k := max(k, c[u]);

        writeln(k);
        for u := 1 to n-1 do write(c[u], ' ');
        writeln(c[n]);

    end;
end.
