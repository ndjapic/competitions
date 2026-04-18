# Задатак: A_Yet_Another_AB_Problem.pas

```pascal
program A_Yet_Another_AB_Problem;
{$H+}
const
    maxn = 200 * 1000;
var
    n, i, a, b, d, ans: int32;
    s, t: string;

function ab(i: int32): boolean;
begin
    ab := (s[i] = 'A') and (t[i] = 'B');
end;

function ba(i: int32): boolean;
begin
    ba := (s[i] = 'B') and (t[i] = 'A');
end;

begin
    readln(n);
    readln(s);
    readln(t);

    ans := 0;

    a := 0;
    i := 1;
    while (i <= n) and not ab(i) do begin
        if t[i] = 'A' then inc(a);
        inc(i);
    end;

    if (i <= n) and (a = 0) then ans := -1;

    if ans > -1 then begin

        b := 0;
        i := n;
        while (i > 0) and not ba(i) do begin
            if t[i] = 'B' then inc(b);
            dec(i);
        end;

        if (i > 0) and (b = 0) then ans := -1;

        if ans > -1 then begin

            d := 0;
            for i := 1 to n do begin
                if ba(i) then
                    inc(d)
                else if ab(i) then begin
                    if d > 0 then dec(d);
                    inc(ans);
                end;
            end;
            inc(ans, d);

        end;

    end;

    writeln(ans);
end.

```
