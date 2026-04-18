# Задатак: B_Rectangle_Filling.pas

```pascal
program B_Rectangle_Filling;
{H+}
uses
    math;
const
    sz = 500;
var
    ntc, tci: int16;
    n, m, i, j: int16;
    ans: boolean;
    s: array [1 .. sz] of string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);
        for i := 1 to n do readln(s[i]);
        ans := false;

        if not ans then begin
            i := 1;
            j := 1;
            while (i <= n) and (s[i][m] <> s[1][1]) do inc(i);
            while (j <= m) and (s[n][j] <> s[1][1]) do inc(j);
            ans := (i <= n) and (j <= m);
        end;

        if not ans then begin
            i := 1;
            j := 1;
            while (i <= n) and (s[i][1] <> s[1][m]) do inc(i);
            while (j <= m) and (s[n][j] <> s[1][m]) do inc(j);
            ans := (i <= n) and (j <= m);
        end;

        if not ans then begin
            i := 1;
            j := 1;
            while (i <= n) and (s[i][m] <> s[n][1]) do inc(i);
            while (j <= m) and (s[1][j] <> s[n][1]) do inc(j);
            ans := (i <= n) and (j <= m);
        end;

        if not ans then begin
            i := 1;
            j := 1;
            while (i <= n) and (s[i][1] <> s[n][m]) do inc(i);
            while (j <= m) and (s[1][j] <> s[n][m]) do inc(j);
            ans := (i <= n) and (j <= m);
        end;

        if ans then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
