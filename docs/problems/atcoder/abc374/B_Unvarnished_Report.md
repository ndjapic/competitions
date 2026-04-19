# Problem: B_Unvarnished_Report.pas

```pascal
program B_Unvarnished_Report;
{$mode delphi}
var
    s, t: string;
    n, m, i: int8;

begin
    readln(s);
    readln(t);
    n := length(s);
    m := length(t);

    i := 1;
    while (i <= n) and (i <= m) and (s[i] = t[i]) do inc(i);
    if (i > n) and (i > m) then i := 0;
    writeln(i);
end.

```
