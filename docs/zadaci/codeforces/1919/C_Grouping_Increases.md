# Задатак: C_Grouping_Increases.pas

```pascal
program C_Grouping_Increases;
const
    maxn = 200 * 1000;
type
    tarr = array [1 .. maxn] of int32;
var
    ntc, tci: int16;
    n, i, x, ns, nt, penalty: int32;
    s, t: tarr;

procedure append(var b: tarr; var nb: int32; x: int32);
begin
    if (nb > 0) and (b[nb] < x) then inc(penalty);
    inc(nb);
    b[nb] := x;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        ns := 0;
        nt := 0;
        penalty := 0;

        for i := 1 to n do begin
            read(x);
            if (ns = 0) or (nt = 0) and (x <= s[ns]) then
                append(s, ns, x)
            else if nt = 0 then
                append(t, nt, x)
            else if s[ns] < t[nt] then begin
                if (s[ns] < x) and (x <= t[nt]) then
                    append(t, nt, x)
                else
                    append(s, ns, x);
            end else begin
                if (t[nt] < x) and (x <= s[ns]) then
                    append(s, ns, x)
                else
                    append(t, nt, x);
            end;
        end;
        readln;

        writeln(penalty);

    end;
end.

```
