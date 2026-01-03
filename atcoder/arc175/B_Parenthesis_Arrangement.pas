program B_Parenthesis_Arrangement;
{$H+}
uses
    math;
var
    n, i, a, b, cl, cr, h, mn: int32;
    cost: int64;
    s: string;

begin
    readln(n, a, b);
    readln(s);

    cl := 0;
    cr := 0;
    for i := 1 to 2*n do
        case s[i] of
            '(': inc(cl);
            ')': inc(cr);
        end;

    cost := 0;

    for i := 1 to 2*n do
        if (cr > n) and (s[i] = ')') then begin
            s[i] := '(';
            dec(cr);
            inc(cl);
            inc(cost, b);
        end;

    for i := 2*n downto 1 do
        if (cl > n) and (s[i] = '(') then begin
            s[i] := ')';
            dec(cl);
            inc(cr);
            inc(cost, b);
        end;

    h := 0;
    mn := 0;
    for i := 1 to 2*n do
        case s[i] of
            '(': inc(h);
            ')': begin
                dec(h);
                mn := min(mn, h);
            end;
        end;

    inc(cost, int64(-mn) * min(a, 2*b));
    writeln(cost);
end.
