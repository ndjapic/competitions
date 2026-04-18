# Задатак: A_Creating_Words.pas

```pascal
program A_Creating_Words;
{$mode objfpc}{$H+}{$J-}
var
    ntc, tci: int8;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(s);

        s[4] := s[5];
        s[5] := s[1];
        s[1] := s[4];
        s[4] := ' ';

        writeln(s);

    end;
end.

```
