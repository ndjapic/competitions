# Задатак: B_Daily_Cookie_2.pas

```pascal
program B_Daily_Cookie_2;
{$mode delphi}
var
    n, d, i: int8;
    s: string;

begin
    readln(n, d);
    readln(s);

    for i := n downto 1 do
        if (d > 0) and (s[i] = '@') then begin
            s[i] := '.';
            dec(d);
        end;

    writeln(s);
end.

```
