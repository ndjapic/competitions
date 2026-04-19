# Problem: A_Required_Length.pas

```pascal
program A_Required_Length;
{$MODE DELPHI}
var
    p: string;
    l: int8;

begin
    readln(p);
    readln(l);

    if length(p) >= l then
        writeln('Yes')
    else
        writeln('No');
end.

```
