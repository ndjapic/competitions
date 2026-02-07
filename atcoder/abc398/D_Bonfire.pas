program D_Bonfire;
{$MODE DELPHI}
const
    nn = 200 * 1000;
var
    n, k, r, c: int32;
    s, a: string;

begin
    readln(n, r, c);
    readln(s);
    setlength(a, n);

    for k := 1 to n do
        case s[k] of
            'N': inc(r);
            'W': inc(c);
            'S': dec(r);
            'E': dec(c);
        end;

    for k := 1 to n do begin

        if (r = 0) and (c = 0) then
            a[n+1-k] := '1'
        else
            a[n+1-k] := '0';

        case s[k] of
            'N': dec(r);
            'W': dec(c);
            'S': inc(r);
            'E': inc(c);
        end;

    end;

    writeln(a);
end.
