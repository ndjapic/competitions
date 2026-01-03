program B_Shrink;
var
    ntc, tci, n, i: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 2 to n do write(i, ' ');
        writeln('1');

    end;
end.
