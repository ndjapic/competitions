# Problem: C_XOR_distance.pas

```pascal
program C_XOR_distance;
var
    ntc, tci: int16;
    a, b, r, x, p2, d: int64;

function xord(a, b, x: int64): int64;
begin
    xord := abs((a xor x) - (b xor x));
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a, b, r);

        x := 0;
        p2 := int64(1) shl 59;
        d := xord(a, b, x);

        while p2 > 0 do begin

            inc(x, p2);
            if (x <= r) and (d > xord(a, b, x)) then
                d := xord(a, b, x)
            else
                dec(x, p2);
            p2 := p2 div 2;

        end;

        writeln(d);

    end;
end.

```
