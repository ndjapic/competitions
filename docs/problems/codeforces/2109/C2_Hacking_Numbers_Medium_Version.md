# Problem: C2_Hacking_Numbers_Medium_Version.pas

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
        writeln('digit'); flush(output); readln(o);
        if n <> 9 then begin
            writeln('add ', n-9); flush(output); readln(o);
        end;
        writeln('!'); flush(output); readln(o);

    end;
end.

```
