program A_Dinner_Time;
var
    ntc, tci: int16;
    n, m, p, q: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, p, q);

        if n mod p > 0 then
            writeln('YES')
        else if m mod q > 0 then
            writeln('NO')
        else if n div p = m div q then
            writeln('YES')
        else
            writeln('NO');

    end;
end.
