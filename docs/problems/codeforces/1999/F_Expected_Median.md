# Problem: F_Expected_Median.pas

```pascal
program F_Expected_Median;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
	nn = 200 * 1000;
	prime = 1000 * 1000 * 1000 + 7;
var
    ntc, tci: int16;
    n, k, i, ai, c0, c1, s: int32;
    fact, invf: array [0 .. nn] of int32;

function modmul(a, b: int32): int32;
begin
    modmul := int64(a) * b mod prime;
end;

function modpow(b, e: int32): int32;
begin
    if e = 0 then
        modpow := 1
    else if odd(e) then
        modpow := modmul(b, modpow(b, e-1))
    else
        modpow := modpow(modmul(b, b), e div 2);
end;

function ncr(n, r: int32): int32;
begin
    ncr := modmul(modmul(invf[r], invf[n-r]), fact[n]);
end;

begin
    fact[0] := 1;
    for n := 1 to nn do fact[n] := modmul(n, fact[n-1]);
    invf[nn] := modpow(fact[nn], prime - 2);
    for n := nn downto 1 do invf[n-1] := modmul(n, invf[n]);

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

		c1 := 0;
		for i := 1 to n do begin
			read(ai);
			inc(c1, ai);
		end;
		c0 := n-c1;
		readln;

		s := 0;
		for i := max(0, k-c1) to min(k div 2, c0) do
			if (i <= c0) and (k-i <= c1) then
				s := (int64(ncr(c0, i)) * ncr(c1, k-i) + s) mod prime;

		writeln(s);

    end;
end.

```
