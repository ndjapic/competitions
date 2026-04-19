# Problem: A_Conflict.pas

```pascal
program A_Conflict;
{$MODE DELPHI}
var
    n, i: int8;
    t, a: string;

begin
    readln(n);
    readln(t);
    readln(a);

    i := 1;
    while (i <= n) and ((t[i] = 'x') or (a[i] = 'x')) do inc(i);

    if i <= n then
        writeln('Yes')
    else
        writeln('No');
end.

```
