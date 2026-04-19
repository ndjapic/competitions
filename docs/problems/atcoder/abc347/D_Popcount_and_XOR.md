# Problem: D_Popcount_and_XOR.pas

```pascal
program D_Popcount_and_XOR;
uses
    math;
var
    a, b: int8;
    c, x, y, p2: int64;

begin
    readln(a, b, c);

    x := 0;
    y := 0;

    p2 := 1;
    while p2 shr 60 = 0 do begin
        if (c and p2 > 0) and (max(a, b) > 0) then begin
            if a > b then begin
                inc(x, p2);
                dec(a);
            end else begin
                inc(y, p2);
                dec(b);
            end;
        end;
        inc(p2, p2);
    end;

    p2 := 1;
    while p2 shr 60 = 0 do begin
        if (c and p2 = 0) and (min(a, b) > 0) then begin
            inc(x, p2);
            dec(a);
            inc(y, p2);
            dec(b);
        end;
        inc(p2, p2);
    end;

    if (x xor y = c) and (max(a, b) = 0) then
        writeln(x, ' ', y)
    else
        writeln(-1);
end.

```
