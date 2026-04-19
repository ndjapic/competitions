# Problem: B_Binary_Colouring.pas

```pascal
program B_Binary_Colouring;
const
    nn = 32;
var
    ntc, tci: int16;
    x: int32;
    n, i: int8;
    a: array [0 .. nn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(x);

        for n := 0 to nn-1 do a[n] := 0;

        n := 0;
        while x > 0 do begin
            a[n] := 0;
            if x mod 4 = 3 then begin
                a[n] := -1;
                inc(x);
            end;
            inc(a[n], x mod 2);
            x := x div 2;
            inc(n);
        end;

        writeln(n);
        for i := 0 to n-2 do write(a[i], ' '); writeln(a[n-1]);

    end;
end.

```
