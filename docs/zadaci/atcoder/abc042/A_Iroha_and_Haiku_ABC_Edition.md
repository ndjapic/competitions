# Задатак: A_Iroha_and_Haiku_ABC_Edition.pas

```pascal
program A_Iroha_and_Haiku_ABC_Edition;
var
    a, b, c: int8;

function haiku(a, b, c: int8): boolean;
begin
    haiku := (a = 5) and (b = 7) and (c = 5);
end;

begin
    readln(a, b, c);

    if haiku(a, b, c) then
        writeln('YES')
    else if haiku(b, c, a) then
        writeln('YES')
    else if haiku(c, a, b) then
        writeln('YES')
    else
        writeln('NO');
end.

```
