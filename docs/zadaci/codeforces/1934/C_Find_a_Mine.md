# Задатак: C_Find_a_Mine.pas

```pascal
program C_Find_a_Mine;
var
    ntc, tci: int16;
    n, m, d, x1, y1, x2, y2: int32;

function ask(x, y: int32): int32;
var
    d: int32;
begin
    writeln('? ', x, ' ', y);
    flush(output);
    readln(d);
    ask := d;
end;

procedure say(x, y: int32);
begin
    writeln('! ', x, ' ', y);
    flush(output);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

        d := ask(1, 1);

        x1 := 1;
        y1 := 1 + d;
        if y1 > m then begin
            inc(x1, y1-m);
            y1 := m;
        end;

        x2 := 1 + d;
        y2 := 1;
        if x2 > n then begin
            inc(y2, x2-n);
            x2 := n;
        end;

        d := ask(x1, y1) div 2;
        inc(x1, d);
        dec(y1, d);

        d := ask(x2, y2) div 2;
        dec(x2, d);
        inc(y2, d);

        d := ask(x1, y1);
        if d = 0 then
            say(x1, y1)
        else
            say(x2, y2);

    end;
end.

```
