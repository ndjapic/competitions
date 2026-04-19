# Problem: A_Rating_Increase.pas

```pascal
program A_Rating_Increase;
var
    ntc, tci, ab, a, b, p0, p1: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(ab);
        p1 := 1;

        repeat
            p0 := p1;
            p1 := p1 * 10;
            a := ab div p1;
            b := ab mod p1;
        until (a = 0) or (a < b) and (b >= p0);

        if a = 0 then
            writeln(-1)
        else
            writeln(a, ' ', b);

    end;
end.

```
