program C_Clock_and_Strings;
uses
    math;
var
    ntc, tci: int16;
    a, b, c, d: int8;

procedure swp2(var a, b: int8);
var
    x: int8;
begin
    x := a;
    a := b;
    b := x;
end;

procedure swp4(var a, b, c, d: int8);
begin
    swp2(a, c);
    swp2(b, d);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a, b, c, d);

        if a > b then swp2(a, b);
        if c > d then swp2(c, d);
        if a > c then swp4(a, b, c, d);

        if (c < b) and (b < d) then
            writeln('YES')
        else
            writeln('NO');

    end;
end.
