# Задатак: B_Ticket_Gate_Log.pas

```pascal
program B_Ticket_Gate_Log;
{$MODE DELPHI}
var
    n, i, ans: int8;
    s: string;

begin
    readln(s);
    n := length(s);

    ans := 0;
    for i := 2 to n do
        if s[i] = s[i-1] then inc(ans);

    if s[1] = 'o' then inc(ans);
    if s[n] = 'i' then inc(ans);

    writeln(ans);
end.

```
