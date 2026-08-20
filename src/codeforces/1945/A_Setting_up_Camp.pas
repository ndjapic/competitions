program A_Setting_up_Camp;
var
    ntc, tci: int16;
    a, b, c, m: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a, b, c);

        m := b mod 3;

        if (m = 1) and (c < 2) or (m = 2) and (c < 1) then
            writeln(-1)
        else
            writeln(a + (b+c+2) div 3);

    end;
end.
