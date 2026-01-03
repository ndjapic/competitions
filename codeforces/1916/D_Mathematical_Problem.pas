program D_Mathematical_Problem;
var
    ntc, tci: int8;
    n, h, i: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);

        if n = 1 then
            writeln(1)
        else begin

            for h := 0 to (n-3) div 2 do begin

                write('1');
                for i := 1 to h do write('0');
                write('6');
                for i := 1 to h do write('0');
                write('9');
                for i := 1 to n-3-2*h do write('0');
                writeln;

                write('9');
                for i := 1 to h do write('0');
                write('6');
                for i := 1 to h do write('0');
                write('1');
                for i := 1 to n-3-2*h do write('0');
                writeln;

            end;

            write('196');
            for i := 1 to n-3 do write('0');
            writeln;

        end;

    end;
end.
