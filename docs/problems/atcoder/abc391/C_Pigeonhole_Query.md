# Problem: C_Pigeonhole_Query.pas

```pascal
program C_Pigeonhole_Query;
{$mode delphi}{$inline on}
const
    nn = 1000 * 1000;
var
    n, q, i, k, p, h, ans: int32;
    qt: int8;
    loc, c: array [1 .. nn] of int32;

begin
    readln(n, q);

    for i := 1 to n do begin
        loc[i] := i;
        c[i] := 1;
    end;
    ans := 0;

    for k := 1 to q do begin
        read(qt);
        case qt of

            1: begin
                read(p, h);

                i := loc[p];
                dec(c[i]);
                if c[i] = 1 then dec(ans);

                inc(c[h]);
                if c[h] = 2 then inc(ans);
                loc[p] := h;
            end;

            2: writeln(ans);

        end;
        readln;
    end;
end.

```
