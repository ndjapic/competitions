# Задатак: A_Tomorrow.pas

```pascal
program A_Tomorrow;
var
    mm, dd, y, m, d: int32;

begin
    readln(mm, dd);
    readln(y, m, d);

    inc(d);
    if d > dd then begin
        d := 1;
        inc(m);
        if m > mm then begin
            m := 1;
            inc(y);
        end;
    end;

    writeln(y, ' ', m, ' ', d);
end.

```
