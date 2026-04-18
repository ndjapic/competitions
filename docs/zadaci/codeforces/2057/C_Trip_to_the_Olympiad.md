# Задатак: C_Trip_to_the_Olympiad.pas

```pascal
program C_Trip_to_the_Olympiad;
uses
    math;
var
    ntc, tci: int16;
    l, r, a, b, c, p2: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(l, r);

        a := l;
        b := 0;
        c := r;

        p2 := int32(1) shl 29;
        while a and p2 = c and p2 do begin
            inc(b, a and p2);
            p2 := p2 div 2;
        end;

        while p2 > 0 do begin
            if (a and p2 = 0) and (c and p2 = 0) and (b+p2 < c) then
                inc(b, p2);
            p2 := p2 div 2;
        end;

        p2 := int32(1) shl 29;
        while (b <= a) and (p2 > 0) do begin
            if (b and p2 = 0) and (a and p2 <> c and p2) and (b+p2 < c) then
                inc(b, p2);
            p2 := p2 div 2;
        end;

        p2 := 1;
        while b <= a do begin
            if b and p2 = 0 then
                inc(b, p2);
            inc(p2, p2);
        end;

        writeln(a, ' ', b, ' ', c);

    end;
end.

```
