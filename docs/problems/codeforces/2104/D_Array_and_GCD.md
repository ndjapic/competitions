# Problem: D_Array_and_GCD.pas

```pascal
program D_Array_and_GCD;
uses
    math;
const
    nn = 400 * 1000;
    pp = 5800 * 1000 + 79; (* The 400,000th prime is 5,800,079. *)
var
    ntc, tci: int16;
    n, i, p: int32;
    a, cp: array [1 .. nn] of int32;
    isPrime: array [2 .. pp] of boolean;
    s, primes_sum: array [0 .. nn] of int64;

procedure msort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        il := l;
        ir := m;
        for i := l to r-1 do
            if (ir >= r) or (il < m) and (a[il] >= a[ir]) then begin
                cp[i] := a[il];
                inc(il);
            end else begin
                cp[i] := a[ir];
                inc(ir);
            end;

        for i := l to r-1 do a[i] := cp[i];

    end;
end;

begin
    for p := 2 to pp do isPrime[p] := true;

    p := 2;
    n := p*p;
    while n <= pp do begin
        if isPrime[p] then begin
            while n <= pp do begin
                isPrime[n] := false;
                inc(n, p);
            end;
        end;
        inc(p);
        n := p*p;
    end;

    p := 2;
    primes_sum[0] := 0;
    for i := 1 to nn do begin
        while not isPrime[p] do inc(p);
        primes_sum[i] := primes_sum[i-1] + p;
        inc(p);
    end;

    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);
        for i := 1 to n do read(a[i]); readln; msort(1, n+1);

        s[0] := 0;
        for i := 1 to n do s[i] := s[i-1] + a[i];

        i := n;
        while s[i] < primes_sum[i] do dec(i);

        writeln(n-i);

    end;
end.

```
