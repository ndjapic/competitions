# Задатак: A_MEX_Table.pas

```pascal
program A_MEX_Table;
uses
    math;
var
    ntc, tci: int16;
    n, m: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);
        writeln(max(n, m)+1);

    end;
end.

```
