# Problem: A_Who_Ate_the_Cake.pas

```pascal
program A_Who_Ate_the_Cake;
uses
    math;
var
    a, b, c: int8;

begin
    readln(a, b);

    c := -1;
    if a <> b then begin
        c := 1;
        if c = min(a, b) then inc(c);
        if c = max(a, b) then inc(c);
    end;

    writeln(c);
end.

```
