# Problem: E_Binary_Search.pas

```pascal
program E_Binary_Search;
uses
    math;
const
    sz = 200 * 1000;
var
    ntc, tci: int16;
    n, x, i, l, r, m: int32;
    p: array [0 .. sz] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, x);

        for i := 1 to n do read(p[i]); readln;


    end;
end.

```
