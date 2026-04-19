# Problem: A_Leftrightarrow.pas

```pascal
program B_Make_Many_Triangles;
{$H+}
var
    n, i: int8;
    s: string;

begin
    readln(s);
    n := length(s);

    i := 2;
    while (i < n) and (s[i] = '=') do inc(i);

    if (i = n) and (s[1] = '<') and (s[n] = '>') then
        writeln('Yes')
    else
        writeln('No');
end.

```
