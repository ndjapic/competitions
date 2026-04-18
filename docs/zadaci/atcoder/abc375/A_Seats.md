# Задатак: A_Seats.pas

```pascal
program A_Seats;
{$mode delphi}
var
    n, i, ans: int32;
    s: string;

begin
    readln(n);
    readln(s);


    ans := 0;
    for i := 1 to n-2 do
        if (s[i] = '#') and (s[i+1] = '.') and (s[i+2] = '#') then
            inc(ans);

    writeln(ans);
end.

```
