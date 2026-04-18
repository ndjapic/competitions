# Задатак: B_Aleksa_and_Stack.pas

```pascal
program B_Aleksa_and_Stack;
var
    ntc, tci: int16;
    n, i: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n-1 do write(i+4, ' ');
        writeln(n+4);

    end;
end.


```
