# Problem: A_The_bottom_of_the_ninth.pas

```pascal
program A_The_bottom_of_the_ninth;
uses
    math;
var
    i, r: int8;
    a, b: int16;

begin
    a := 0;
    for i := 1 to 9 do begin
        read(r);
        inc(a, r);
    end;
    readln;

    b := 0;
    for i := 1 to 8 do begin
        read(r);
        inc(b, r);
    end;
    readln;

    writeln(a+1-b);
end.

```
