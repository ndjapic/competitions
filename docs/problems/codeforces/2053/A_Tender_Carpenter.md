# Problem: A_Tender_Carpenter.pas

```pascal
program A_Tender_Carpenter;
uses
    math;
const
    nn = 200;
var
    ntc, tci, n, i: int16;
    mn, mx: int32;
    a: array [1 .. nn] of int32;
    found: boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(a[i]); readln;

        found := false;
        i := 2;
        while not found and (i <= n) do begin
            mn := min(a[i-1], a[i]);
            mx := max(a[i-1], a[i]);
            found := 2*mn > mx;
            inc(i);
        end;

        if found then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
