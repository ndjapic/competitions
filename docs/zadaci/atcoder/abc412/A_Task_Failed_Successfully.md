# Задатак: A_Task_Failed_Successfully.pas

```pascal
program A_Task_Failed_Successfully;
var
    n, i, ai, bi, days: int8;

begin
    readln(n);

    days := 0;
    for i := 1 to n do begin
        readln(ai, bi);
        if bi > ai then inc(days);
    end;

    writeln(days);
end.

```
