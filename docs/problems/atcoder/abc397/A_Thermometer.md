# Problem: A_Thermometer.pas

```pascal
program A_Thermometer;
var
    x: real;

begin
    readln(x);

    if x >= 38.0 then
        writeln(1)
    else if x >= 37.5 then
        writeln(2)
    else
        writeln(3);
end.

```
