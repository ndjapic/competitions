program Prime_pair_connection;
uses
    math;
const
    maxd = 1000 * 2000;
var
    ntc, tci: int8;
    l, r: int32;
    n, r1, p, p1, p2, pow10, c, x, s, ans: qword;
    is_prime: array [0 .. maxd] of boolean;

function modpow(b, e, p: qword): qword;
begin
    if e = 0 then
        modpow := 1
    else if odd(e) then
        modpow := b * modpow(b, e-1, p) mod p
    else
        modpow := modpow(b * b mod p, e div 2, p);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(l, r);
        r1 := l + maxd;
        for n := l to r1 do is_prime[n-l] := true;

        p := 2;
        n := p*p;
        while n <= r1 do begin
            if n < l then n := (l+p-1) div p * p;
            while n <= r1 do begin
                is_prime[n-l] := false;
                inc(n, p);
            end;
            inc(p);
            n := p*p;
        end;

        p1 := l;
        while (p1 <= r) and not is_prime[p1-l] do inc(p1);

        pow10 := 1;
        ans := 0;

        while p1 <= r do begin
            p2 := p1+1;
            while not is_prime[p2-l] do inc(p2);

            while pow10 <= p1 do pow10 := pow10 * 10;

            c := pow10 mod p2;
            x := int64(p2-p1) * modpow(c, p2-2, p2) mod p2;
            s := p1 + pow10 * x;

            inc(ans, s);

            p1 := p2;
        end;

        writeln(ans);

    end;
end.
