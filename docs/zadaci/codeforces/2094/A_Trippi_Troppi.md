# Задатак: A_Trippi_Troppi.pas

```pascal
program A_Trippi_Troppi;
{$MODE DELPHI}
var
    ntc, tci, n, i: int8;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(s);
        n := length(s);

        for i := 1 to n do
            if (i = 1) or (s[i-1] = ' ') then write(s[i]);
        writeln;

    end;
end.

```
