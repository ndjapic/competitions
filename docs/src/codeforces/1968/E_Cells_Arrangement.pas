program E_Cells_Arrangement;
const
    nn = 1000;
var
    ntc, tci: int8;
    n, i: int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        writeln('1 1');
        writeln('1 2');
        for i := 3 to n do writeln(i, ' ', i);

    end;
end.
