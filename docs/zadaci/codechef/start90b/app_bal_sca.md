# Задатак: app_bal_sca.pas

```pascal
program app_bal_sca;
var
    notc: int32;
    m, n: int64;

begin
    readln(notc);
    repeat

        readln(m, n);

        if n > m then
            writeln('NO')
        else begin

            while not odd(m) do m := m div 2;

            if n mod m = 0 then
                writeln('YES')
            else
                writeln('NO');

        end;

        dec(notc);
    until notc = 0;
end.


```
