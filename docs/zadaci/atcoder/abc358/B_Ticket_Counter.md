# Задатак: B_Ticket_Counter.pas

```pascal
program B_Ticket_Counter;
{$mode objfpc}{$H+}{$J-}
uses
    math;
var
    n, i: int8;
    a: int32;
    t: array of int32;

begin
    readln(n, a);
    setlength(t, n+1);

    for i := 1 to n do begin
        read(t[i]);
        if i > 1 then
            t[i] := max(t[i], t[i-1]);
        inc(t[i], a);
        writeln(t[i]);
    end;
end.

```
