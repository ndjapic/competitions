# Задатак: A_Rectangle_Cutting.pas

```pascal
program A_Rectangle_Cutting;
var
    ntc, tci, a, b: int32;

procedure solve(a, b: int32);
begin
    if not odd(b) then
        writeln('Yes')
    else if a = 2*b then
        writeln('No')
    else if odd(a) then
        writeln('No')
    else
        writeln('Yes');
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(a, b);

        if a > b then
            solve(a, b)
        else
            solve(b, a);
    end;
end.

```
