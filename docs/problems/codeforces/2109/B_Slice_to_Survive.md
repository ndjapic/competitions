# Problem: B_Slice_to_Survive.pas

```pascal
program B_Slice_to_Survive;
uses
    math;
var
    ntc, tci: int16;
    n, m, a, b: int32;

function f(n, m: int32): int8;
begin
    if n > m then
        f := f(m, n)
    else if m = 1 then
        f := 0
    else
        f := 1 + f(n, (m+1) div 2);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, a, b);

        writeln(
            min(
                min(f(a, m), f(n-a+1, m)),
                min(f(n, b), f(n, m-b+1))
            ) + 1
        );

    end;
end.

```
