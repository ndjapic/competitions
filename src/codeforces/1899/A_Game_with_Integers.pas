program A_Game_with_Integers;
const
    maxn = 1000;
var
    ntc, tci: int8;
    n: int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n);
        if n mod 3 > 0 then
            writeln('First')
        else
            writeln('Second');
    end;
end.
