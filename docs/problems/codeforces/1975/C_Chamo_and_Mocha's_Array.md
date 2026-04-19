# Problem: C_Chamo_and_Mocha's_Array.pas

```pascal
program C_Chamo_and_Mochas_Array;
uses
    math;
const
    nn = 100 * 1000;
var
    ntc, tci: int16;
    n, i, m, x, y, z: int32;
    a: array [1 .. nn] of int32;

procedure swp(var u, v: int32);
var
    w: int32;
begin
    if u > v then begin
        w := u;
        u := v;
        v := w;
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(a[i]);
        readln;

        m := 0;
        for i := 2 to n do m := max(m, min(a[i-1], a[i]));
        for i := 2 to n-1 do begin
            x := a[i-1];
            y := a[i];
            z := a[i+1];
            swp(x, y);
            swp(x, z);
            swp(y, z);
            m := max(m, y);
        end;

        writeln(m);

    end;
end.

```
