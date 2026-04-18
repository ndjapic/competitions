# Задатак: D_Logical_Filling.pas

```pascal
program D_Logical_Filling;
{$MODE DELPHI}
var
    n, i, k, q: int32;
    s, t: string;
    can_o: boolean;

begin
    readln(n, k);
    readln(s);
    setlength(t, n);

    q := 0;
    for i := 1 to n do begin
        t[i] := s[i];
        if s[i] = 'o' then dec(k);
        if s[i] = '?' then inc(q);
    end;

    for i := 1 to n do begin
        can_o := (s[i] = '?') and (k > 0) and (
            (i = 1) or (s[i-1] <> 'o')
        ) and (
            (i = n) or (s[i+1] <> 'o')
        );

        if not can_o then
            t[i] := '.'
        else if q >= k then
            t[i] := 'o';
    end;

    writeln(t);
end.

```
