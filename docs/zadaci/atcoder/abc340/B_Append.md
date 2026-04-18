# Задатак: B_Append.pas

```pascal
program B_Append;
const
    maxq = 100;
var
    q, i, n, k, tp: int8;
    x: array [1 .. maxq] of int32;

begin
    readln(q);
    n := 0;

    for i := 1 to q do begin
        read(tp);
        case tp of

            1: begin
                inc(n);
                readln(x[n]);
            end;

            2: begin
                readln(k);
                writeln(x[n+1-k]);
            end;

        end;
    end;
end.

```
