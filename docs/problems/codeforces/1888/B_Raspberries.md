# Problem: B_Raspberries.pas

```pascal
program B_Raspberries;
uses
    math;
const
    maxn = 100 * 1000;
var
    ntc, tci, n, i: int32;
    k, e, x, ans: int8;
    a: array [1 .. maxn] of int8;

function need(x, p: int8): int8;
begin
    need := (x+p-1) div p * p - x;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        for i := 1 to n do read(a[i]); readln;

        for i := n downto 2 do
            if need(a[i-1], k) > need(a[i], k) then begin
                x := a[i-1];
                a[i-1] := a[i];
                a[i] := x;
            end;

        ans := need(a[1], k);

        if k = 4 then begin

            for e := 1 to 2 do
                for i := n downto 2 do
                    if need(a[i-1], 2) > need(a[i], 2) then begin
                        x := a[i-1];
                        a[i-1] := a[i];
                        a[i] := x;
                    end;

            ans := min(ans, need(a[1], 2) + need(a[2], 2));

        end;

        writeln(ans);

    end;
end.

```
