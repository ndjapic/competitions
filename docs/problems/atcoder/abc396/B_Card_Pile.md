# Problem: B_Card_Pile.pas

```pascal
program B_Card_Pile;
const
    nn = 200 * 1000;

var
    q, i, t, c: int8;
    s: array [1 .. nn] of int8;

begin
    readln(q);
    t := 0;

    for i := 1 to q do begin
        read(c);
        case c of

            1: begin
                inc(t);
                read(s[t]);
            end;

            2: begin
                writeln(s[t]);
                dec(t);
            end;

        end;
        readln;
    end;
end.

```
