# Задатак: B_Typing.pas

```pascal
program B_Typing;
{$MODE DELPHI}
var
    n, m, i, j: int32;
    s, t: string;

begin
    readln(s);
    readln(t);
    n := length(s);
    m := length(t);

    i := 1;
    for j := 1 to m do
        if s[i] = t[j] then begin
            write(j, ' ');
            inc(i);
        end;
    writeln;
end.

```
