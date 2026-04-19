# Problem: D_Masked_Popcount.pas

```pascal
program D_Masked_Popcount;
uses
    math;
const
    ee = 60;
    prime = 998244353;
var
    n, m, p0, p1, ans: int64;
    e: int8;

begin
    readln(n, m);
    inc(n);

    ans := 0;
    p0 := 1;
    p1 := 2;
    for e := 0 to ee-1 do begin
        if odd(m shr e) then
            ans := (ans + n div p1 * p0 + max(0, n mod p1 - p0)) mod prime;
        p0 := p1;
        inc(p1, p1);
    end;

    writeln(ans);
end.

```
