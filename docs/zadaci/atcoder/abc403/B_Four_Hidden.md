# Задатак: B_Four_Hidden.pas

```pascal
program B_Four_Hidden;
{$MODE DELPHI}
var
    n, m, i, j: int32;
    found: boolean;
    t, u: string;

begin
    readln(t);
    readln(u);
    n := length(t);
    m := length(u);

    found := false;
    for i := 0 to n-1 do
        if not found then begin
            found := true;
            for j := 1 to m do
                if found then found := (t[i+j] = '?') or (t[i+j] = u[j]);
        end;

    if found then
        writeln('Yes')
    else
        writeln('No');
end.

```
