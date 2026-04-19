# Problem: A_Energy_Crystals.pas

```pascal
program A_Energy_Crystals;
uses
    math;
var
    ntc, tci, x, i: int32;
    a: array of int32;

begin
    setlength(a, 3);
    a[0] := 0;
    a[1] := 0;
    a[2] := 0;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(x);
        i := 3;
        while a[i-3] < x do begin
            if length(a) = i then setlength(a, 2*i);
            a[i] := min(x, 2*a[i-2]+1);
            inc(i);
        end;

        writeln(i-3);

    end;
end.

```
