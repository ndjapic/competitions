# Problem: C_Equal_Values.pas

```pascal
program C_Equal_Values;
uses
    math;
const
    nn = 500 * 1000;
    inf = 1000 * 1000 * 1000 * 1000 * 1000 * 1000;
var
    ntc, tci, n, l, r: int32;
    cost: int64;
    a: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        cost := inf;
        l := 1;
        for r := 1 to n do begin
            read(a[r]);
            if a[r] <> a[l] then l := r;
            cost := min(cost, int64(n - (r-l+1)) * a[r]);
        end;
        readln;

        writeln(cost);

    end;
end.

```
