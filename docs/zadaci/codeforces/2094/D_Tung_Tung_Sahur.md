# Задатак: D_Tung_Tung_Sahur.pas

```pascal
program D_Tung_Tung_Sahur;
{$MODE DELPHI}
var
    ntc, tci: int16;
    n, m, i, j, i0, j0: int32;
    p, s: string;
    ans: boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(p);
        readln(s);
        n := length(p);
        m := length(s);

        ans := true;
        i0 := 0;
        j0 := 0;
        for i := 1 to n do
            if (i = n) or (p[i] <> p[i+1]) then begin
                j := j0 + 1;
                while (j < m) and (s[j] = s[j+1]) do inc(j);
                ans := ans
                    and (p[i] = s[j])
                    and (i-i0 <= j-j0)
                    and (j-j0 <= 2*(i-i0));
                i0 := i;
                j0 := j;
            end;

        if ans and (j0 = m) then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
