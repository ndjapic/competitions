# Задатак: D_AtCoder_Wallpaper.pas

```pascal
program D_AtCoder_Wallpaper;
const
    aa = 1000 * 1000 * 1000;
var
    a, b, c, d: int64;
    ans: int64;

begin
    readln(a, b, c, d);
    inc(a, aa);
    inc(b, aa);
    inc(c, aa);
    inc(d, aa);
    ans := 0;

    while a mod 4 <> c mod 4 do begin

        case a mod 4 div 2 of
            0: inc(ans, int64(d-b) div 2 * 3);
            1: inc(ans, int64(d-b) div 2 * 1);
        end;

        if odd(d-b) then begin
            if odd(b) then
                case a mod 4 of
                    0: inc(ans, 1);
                    1: inc(ans, 2);
                    2: inc(ans, 1);
                    3: inc(ans, 0);
                end
            else
                case a mod 4 of
                    0: inc(ans, 2);
                    1: inc(ans, 1);
                    2: inc(ans, 0);
                    3: inc(ans, 1);
                end;
        end;

        inc(a);
    end;

    inc(ans, int64(d-b) * (c-a));
    writeln(ans);
end.

```
