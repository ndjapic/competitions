# Задатак: C_Rotatable_Array.pas

```pascal
program C_Rotatable_Array;
const
    nn = 1000 * 1000;
var
    n, p, q, i, k, x, d: int32;
    tp: int8;
    a: array [0 .. nn] of int32;

begin
    readln(n, q);
    for p := 1 to n do a[p-1] := p;
    d := 0;

    for i := 1 to q do begin

        read(tp);
        case tp of

            1: begin
                read(p, x);
                a[(p-1+d) mod n] := x;
            end;

            2: begin
                read(p);
                writeln(a[(p-1+d) mod n]);
            end;

            3: begin
                readln(k);
                d := (d+k) mod n;
            end;

        end;

    end;
    readln;
end.

```
