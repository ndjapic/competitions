# Problem: B_Milena_and_Admirer.pas

```pascal
program B_Milena_and_Admirer;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, l, r, m: int32;
    ans: int64;
    a: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(a[i]);
        readln;

        ans := 0;
        for i := n-1 downto 1 do begin

            if a[i] > a[i+1] then begin
                l := 1;
                r := a[i];
                while r - l > 1 do begin
                    m := (l+r) div 2;
                    if (a[i]+m-1) div m <= a[i+1] then
                        r := m
                    else
                        l := m;
                end;
                inc(ans, r-1);
                a[i] := a[i] div r;
            end;

        end;

        writeln(ans);
    end;
end.

```
