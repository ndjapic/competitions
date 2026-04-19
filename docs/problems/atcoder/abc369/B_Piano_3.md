# Problem: B_Piano_3.pas

```pascal
program B_Piano_3;
var
    n, i, a, l, r: int8;
    ans: int16;
    s: char;

begin
    readln(n);

    ans := 0;
    l := 0;
    r := 0;

    for i := 1 to n do begin
        readln(a, s, s);

        case s of

            'L': begin
                if l > 0 then inc(ans, abs(a-l));
                l := a;
            end;

            'R': begin
                if r > 0 then inc(ans, abs(a-r));
                r := a;
            end;

        end;
    end;

    writeln(ans);
end.

```
