# Problem: A_Content_Too_Large.pas

```pascal
program A_Content_Too_Large;
var
    n, i, ai: int8;
    s, m: int16;

begin
    readln(n, m);

    s := 0;
    for i := 1 to n do begin
        read(ai);
        inc(s, ai);
    end;
    readln;

    if s <= m then
        writeln('Yes')
    else
        writeln('No');
end.

```
