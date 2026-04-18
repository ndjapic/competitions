# Задатак: A_I_hate_1.pas

```pascal
program A_I_hate_1;
var
    n, i, k: int32;

begin
    readln(n);

    if n = 1 then begin

        writeln(1);
        writeln(1);

    end else begin

        k := n div 2;
        writeln(k);
        for i := 1 to k-1 do write(2*i, ' ');
        writeln(2*k);

    end;
end.

```
