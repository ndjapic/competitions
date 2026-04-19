# Problem: A.pas

```pascal
program A;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci, n, i: int32;
    ans: int64;
    a: array [0 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do begin
            read(a[i]);
        end;
        readln;

        writeln(ans);

    end;
end.

```
