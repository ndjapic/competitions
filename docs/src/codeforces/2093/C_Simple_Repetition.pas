program C_Simple_Repetition;
var
    ntc, tci: int16;
    x, d: int32;
    k: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(x, k);

        if k > 2 then
            writeln('NO')
        else if k = 1 then begin

            if x = 1 then
                writeln('NO')
            else begin

                d := 2;
                while (d*d <= x) and (x mod d > 0) do inc(d);

                if d*d <= x then
                    writeln('NO')
                else
                    writeln('YES');

            end;

        end else if x = 1 then
            writeln('YES')
        else
            writeln('NO');
 
    end;

end.
