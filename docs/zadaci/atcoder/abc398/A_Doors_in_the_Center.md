# Задатак: A_Doors_in_the_Center.pas

```pascal
program A_Doors_in_the_Center;
{$MODE DELPHI}
var
    n, i: int8;
    s: string;

begin
    readln(n);
    setlength(s, n);
    for i := 1 to n do s[i] := '-';
    i := (n+1) div 2;
    s[i] := '=';
    if not odd(n) then s[i+1] := '=';
    writeln(s);
end.

```
