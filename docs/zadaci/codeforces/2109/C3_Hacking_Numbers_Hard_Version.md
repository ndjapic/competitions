# Задатак: C3_Hacking_Numbers_Hard_Version.pas

```pascal
program C3_Hacking_Numbers_Hard_Version;
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
        if n mod (8*9*5*7) = 0 then begin
            writeln('mul ', n div (8*9*5*7)); flush(output); readln(o);
        end else begin
            writeln('digit'); flush(output); readln(o);
            if n <> 9 then begin
                writeln('add ', n-9); flush(output); readln(o);
            end;
        end;
        writeln('!'); flush(output); readln(o);

    end;
end.

```
