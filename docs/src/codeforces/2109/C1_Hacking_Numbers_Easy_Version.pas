program C1_Hacking_Numbers_Easy_Version;
var
    ntc, tci: int16;
    n: int32;
    o: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        writeln('mul 9'); flush(output); readln(o);
        writeln('digit'); flush(output); readln(o);
        writeln('digit'); flush(output); readln(o);
        writeln('div 9'); flush(output); readln(o);
        writeln('mul ', n); flush(output); readln(o);
        writeln('!'); flush(output); readln(o);

    end;
end.
