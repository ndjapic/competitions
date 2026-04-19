# Problem: A_Insert.pas

```pascal
program A_Insert;
{$mode objfpc}{$H+}{$J-}
var
    n, k, x, i, ai: int8;

begin
    readln(n, k, x);

    for i := 1 to n do begin
        read(ai);
        write(ai);
        if i = k then write(' ', x);
        if i < n then write(' ');
    end;
    readln;
    writeln;
end.

```
