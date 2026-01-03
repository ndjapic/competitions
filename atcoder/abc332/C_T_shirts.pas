program C_T_shirts;
{$H+}
uses
    math;
var
    n, m, i, x, c, mn: int32;
    s: string;

begin
    readln(n, m);
    readln(s);

    mn := 0;
    x := m;
    c := 0;

    for i := 1 to n do begin
        case s[i] of

            '0': begin
                x := m;
                c := 0;
            end;

            '1': if x > 0 then
                dec(x)
            else
                dec(c);

            '2': dec(c);

        end;
        mn := min(mn, c);
    end;

    writeln(-mn);
end.
