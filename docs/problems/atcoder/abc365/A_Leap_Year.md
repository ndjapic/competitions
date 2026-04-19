# Problem: A_Leap_Year.pas

```pascal
program A_Leap_Year;
var
    y, ans: int16;

begin
    readln(y);

    if y mod 4 > 0 then
        ans := 365
    else if y mod 100 > 0 then
        ans := 366
    else if y mod 400 > 0 then
        ans := 365
    else
        ans := 366;

    writeln(ans);
end.

```
