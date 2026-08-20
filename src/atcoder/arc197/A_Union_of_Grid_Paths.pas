program A_Union_of_Grid_Paths;
{MODE DELPHI}
uses
    math;
const
    hh = 200 * 1000;
var
    ntc, tci, h, w, n, d, r, i, j, k: int32;
    ans: int64;
    s, x: string;
    mn, mx: array [1 .. hh] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(h, w);
        readln(s);
        n := length(s);
        setlength(x, n);

        d := h-1;
        r := w-1;
        for k := 1 to n do
            case s[k] of
                'D': dec(d);
                'R': dec(r);
                '?': ;
            end;

        for i := 1 to h do begin
            mn[i] := w;
            mx[i] := 1;
        end;
        mn[1] := 1;

        i := 1;
        j := 1;
        for k := 1 to n do begin

            if s[k] <> '?' then
                x[k] := s[k]
            else if d > 0 then begin
                x[k] := 'D';
                dec(d);
            end else
                x[k] := 'R';

            case x[k] of
                'D': inc(i);
                'R': inc(j);
            end;

            mn[i] := min(mn[i], j);
        end;

        i := 1;
        j := 1;
        for k := 1 to n do begin

            if s[k] <> '?' then
                x[k] := s[k]
            else if r > 0 then begin
                x[k] := 'R';
                dec(r);
            end else
                x[k] := 'D';

            case x[k] of
                'D': inc(i);
                'R': inc(j);
            end;

            mx[i] := max(mx[i], j);
        end;

        ans := h;
        for i := 1 to h do
            inc(ans, mx[i] - mn[i]);

        writeln(ans);

    end;
end.
