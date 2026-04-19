# Problem: A_Jellyfish_and_Undertale.pas

```pascal
program A_Jellyfish_and_Undertale;
uses
    math;
const
    maxn = 100;
var
    ntc, tci: int16;
    a, b: int32;
    n, i: int8;
    ans: int64;
    x: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a, b, n);

        ans := b;

        for i := 1 to n do begin
            read(x[i]);
            inc(ans, min(x[i], a-1));
        end;
        readln;

        writeln(ans);

    end;
end.


```
