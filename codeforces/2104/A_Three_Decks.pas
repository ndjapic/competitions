program A_Three_Decks;
var
    ntc, tci: int16;
    a, b, c, s: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(a, b, c);
        s := a+b+c;

        if (s mod 3 = 0) and (b <= s div 3) then
            writeln('YES')
        else
            writeln('NO');
 
    end;
end.
