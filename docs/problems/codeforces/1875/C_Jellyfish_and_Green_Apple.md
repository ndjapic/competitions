# Problem: C_Jellyfish_and_Green_Apple.pas

```pascal
program C_Jellyfish_and_Green_Apple;
uses
    math;
const
    maxn = 50;
var
    ntc, tci: int16;
    n, m, ans: int64;

function f(n, m: int64): int64;
var
    ans: int64;
begin
    n := n mod m;
    if n = 0 then
        ans := 0
    else if odd(m) then begin
        ans := -1;
    end else begin
        m := m div 2;
        ans := f(n, m);
        if ans > -1 then
            ans := ans * 2 + n;
    end;
    f := ans;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

        ans := f(n, m);

        writeln(ans);

    end;
end.


```
