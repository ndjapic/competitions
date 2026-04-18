# Задатак: B_AB_Flipping.pas

```pascal
program B_AB_Flipping;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, l, r: int32;
    s: array [1 .. maxn] of char;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(s[i]);
        readln;

        l := 1;
        r := n;
        while (l < r) and (s[l] <> 'A') do inc(l);
        while (l < r) and (s[r] <> 'B') do dec(r);

        writeln(r-l);
    end;
end.

```
