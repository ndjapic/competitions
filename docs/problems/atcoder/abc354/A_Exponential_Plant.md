# Problem: A_Exponential_Plant.pas

```pascal
program A_Exponential_Plant;
var
    h: int32;
    e: int8;

begin
    readln(h);
    inc(h);

    e := 0;
    while h > 0 do begin
        h := h div 2;
        inc(e);
    end;
    writeln(e);
end.

```
