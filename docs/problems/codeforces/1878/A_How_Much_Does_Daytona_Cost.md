# Problem: A_How_Much_Does_Daytona_Cost.pas

```pascal
program A_How_Much_Does_Daytona_Cost;
const
    maxn = 100;
var
    ntc, tci: int16;
    n, k, l, r, x: int8;
    ans: boolean;
    a, c: array [1 .. maxn] of int8;

begin
    for x := 1 to maxn do c[x] := 0;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        ans := false;

        for r := 1 to n do begin
            read(a[r]);
            for l := r downto 1 do begin
                inc(c[a[l]]);
                ans := ans or (c[k] > r-l+1-c[k]);
            end;
            for l := r downto 1 do dec(c[a[l]]);
        end;
        readln;

        if ans then
            writeln('YES')
        else
            writeln('NO');

    end;
end.


```
