# Problem: A_Takahashi_san_2.pas

```pascal
program A_Takahashi_san_2;
{$mode delphi}
var
    s: string;
    n: int8;

begin
    readln(s);
    n := length(s);

    if (s[n-2] = 's') and (s[n-1] = 'a') and (s[n-0] = 'n') then
        writeln('Yes')
    else
        writeln('No');
end.

```
