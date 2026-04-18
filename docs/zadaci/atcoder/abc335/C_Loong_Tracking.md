# Задатак: C_Loong_Tracking.pas

```pascal
program C_Loong_Tracking;
const
    maxn = 1200 * 1000;
var
    n, i, q, p: int32;
    qt: int8;
    c: char;
    x, y: array [1 .. maxn] of int32;

begin
    readln(n, q);

    for i := 1 to n do begin
        x[i] := n+1-i;
        y[i] := 0;
    end;

    for i := 1 to q do begin
        read(qt);
        case qt of

            1: begin
                read(c);
                readln(c);
                inc(n);
                x[n] := x[n-1];
                y[n] := y[n-1];
                case c of
                    'R': inc(x[n]);
                    'L': dec(x[n]);
                    'U': inc(y[n]);
                    'D': dec(y[n]);
                end;
            end;

            2: begin
                readln(p);
                writeln(x[n+1-p], ' ', y[n+1-p]);
            end;

        end;
    end;
end.

```
