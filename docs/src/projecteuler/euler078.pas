program coin_partitions;
const
    maxn = 60 * 1000;
    prime = 1000 * 1000 * 1000 + 7;
var
    ntc, tci: int8;
    n, k: int32;
    p: array [0 .. maxn] of int32;

function pent(k: int32): int32;
begin
    pent := k * (3*k-1) div 2;
end;

procedure modinc(var a: int32; b: int32);
begin
    inc(a, b);
    if a >= prime then dec(a, prime);
end;

procedure moddec(var a: int32; b: int32);
begin
    dec(a, b);
    if a < 0 then inc(a, prime);
end;

begin
    p[0] := 1;
    for n := 1 to maxn do begin

        p[n] := 0;

        k := 1;
        while n >= pent(k) do begin
            if odd(k) then
                modinc(p[n], p[n-pent(k)])
            else
                moddec(p[n], p[n-pent(k)]);
            inc(k);
        end;

        k := -1;
        while n >= pent(k) do begin
            if odd(k) then
                modinc(p[n], p[n-pent(k)])
            else
                moddec(p[n], p[n-pent(k)]);
            dec(k);
        end;

    end;

    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n);
        writeln(p[n]);
    end;
end.
