# Problem: B_Restaurant_Queue.pas

```pascal
program B_Restaurant_Queue;
const
    nn = 100;
var
    q, i, qt, x, l, r: int8;
    a: array [1 .. nn] of int8;

begin
    readln(q);

    l := 1;
    r := 0;

    for i := 1 to q do begin
        read(qt);
        case qt of

            1: begin
                read(x);
                inc(r);
                a[r] := x;
            end;

            2: begin
                writeln(a[l]);
                inc(l);
            end;

        end;
        readln;
    end;
end.

```
