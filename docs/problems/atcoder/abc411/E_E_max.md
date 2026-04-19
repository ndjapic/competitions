# Problem: E_E_max.pas

```pascal
program E_E_max;
const
    nn = 200 * 1000;
    prime = 998244353;
var
    n, i, k, x, p, num, den: int32;
    j: int8;
    a, cp: array [1 .. 6*nn] of record
        i, x: int32;
        j: int8;
    end;
    inv: array [1 .. 6] of int32;
    len: array [1 .. nn] of int8;

function modpow(b, e: int32): int32;
begin
    if e = 0 then
        modpow := 1
    else if odd(e) then
        modpow := int64(b) * modpow(b, e-1) mod prime
    else
        modpow := modpow(int64(b) * b mod prime, e div 2);
end;

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
            if (ir >= r) or (il < m) and (
                a[il].x >= a[ir].x
            ) then begin
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
    readln(n);

    for j := 1 to 6 do inv[j] := modpow(j, prime-2);

    k := 0;
    p := 1;
    for i := 1 to n do begin
        for j := 1 to 6 do begin
            inc(k);
            a[k].i := i;
            a[k].j := j;
            read(a[k].x);
        end;
        readln;
        p := p * 6 mod prime;
        len[i] := 6;
    end;
    msort(1, 6*n+1);

    den := p;
    num := 0;
    for k := 1 to 6*n do begin
        i := a[k].i;
        x := a[k].x;
        p := int64(p) * inv[len[i]] mod prime;
        num := (int64(p) * x + num) mod prime;
        dec(len[i]);
        p := int64(p) * len[i] mod prime;
    end;

    writeln(int64(num) * modpow(den, prime-2) mod prime);
end.

```
