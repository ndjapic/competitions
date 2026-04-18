# Задатак: A_Daily_Cookie.pas

```pascal
program A_Daily_Cookie;
{$mode delphi}
var
    n, d, i: int8;
    s: string;

begin
    readln(n, d);
    readln(s);

    for i := 1 to n do
        if s[i] = '@' then dec(d);

    writeln(n+d);
end.

```
