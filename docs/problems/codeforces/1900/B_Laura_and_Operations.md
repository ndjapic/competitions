# Problem: B_Laura_and_Operations.pas

```pascal
program B_Laura_and_Operations;
var
    ntc, tci: int32;
    a, b, c: int8;
    ostr: string;

begin
    ostr := '# # #';
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a, b, c);

        if odd(b+c) then
            ostr[1] := '0'
        else
            ostr[1] := '1';

        if odd(c+a) then
            ostr[3] := '0'
        else
            ostr[3] := '1';

        if odd(a+b) then
            ostr[5] := '0'
        else
            ostr[5] := '1';

        writeln(ostr);

    end;
end.

```
