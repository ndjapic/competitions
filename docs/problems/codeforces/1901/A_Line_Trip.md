# Problem: A_Line_Trip.pas

```pascal
program A_Line_Trip;
uses
    math;
const
    maxn = 101;
var
    ntc, tci: int16;
    n, x, i: int8;
    ans: int16;
    a: array [0 .. maxn] of int8;

begin
    a[0] := 0;
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, x);

        ans := 0;
        for i := 1 to n do begin
            read(a[i]);
            ans := max(ans, a[i] - a[i-1]);
        end;
        readln;

        ans := max(ans, 2 * (x - a[n]));
        writeln(ans);

    end;
end.

```
