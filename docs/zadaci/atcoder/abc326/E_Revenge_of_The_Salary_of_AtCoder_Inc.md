# Задатак: E_Revenge_of_The_Salary_of_AtCoder_Inc.pas

```pascal
program E_Revenge_of_The_Salary_of_AtCoder_Inc;
const
    maxn = 300 * 1000;
    prime = 998244353;
var
    n, i, ninv: int32;
    ans: int64;
    a: array [1 .. maxn] of int32;

function modpow(b, e: int32): int32;
begin
    if e = 0 then
        modpow := 1
    else if odd(e) then
        modpow := int64(b) * modpow(b, e-1) mod prime
    else
        modpow := modpow(int64(b) * b mod prime, e div 2);
end;

begin
    readln(n);
    for i := 1 to n do read(a[i]); readln;

    ans := 0;
    ninv := modpow(n, prime - 2);
    for i := n downto 1 do
        ans := ((ans + a[i]) * ninv + ans) mod prime;

    writeln(ans);
end.

```
