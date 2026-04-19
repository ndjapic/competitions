# Problem: B_Strawberries.pas

```pascal
program B_Strawberries;
{$mode delphi}
var
    n, k, l, r, strawberries: int8;
    s: string;

begin
    readln(n, k);
    readln(s);

    l := 0;
    strawberries := 0;
    for r := 1 to n do
        if s[r] = 'X' then
            l := r
        else if r-l = k then begin
            inc(strawberries);
            l := r;
        end;

    writeln(strawberries);
end.

```
