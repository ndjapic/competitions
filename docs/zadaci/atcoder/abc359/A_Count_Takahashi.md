# Задатак: A_Count_Takahashi.pas

```pascal
program A_Count_Takahashi;
{$mode objfpc}{$H+}{$J-}
var
    n, i, ans: int8;
    s: string;

begin
    readln(n);

    ans := 0;
    for i := 1 to n do begin
        readln(s);
        if s[1] = 'T' then inc(ans);
    end;

    writeln(ans);
end.

```
