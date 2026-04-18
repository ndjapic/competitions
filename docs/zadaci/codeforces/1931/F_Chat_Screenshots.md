# Задатак: F_Chat_Screenshots.pas

```pascal
program F_Chat_Screenshots;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, k, i, j, j1, j2: int32;
    ans: boolean;
    a: array of array of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        setlength(a, k+1);
        for i := 1 to k do begin
            setlength(a[i], n+1);
            for j := 0 to n-1 do read(a[i][j]);
            readln;
        end;

        j1 := n;
        ans := false;
        while (j1 > 0) and not ans do begin

            a[1][j1] := a[1][0];
            ans := true;

            i := 2;
            while (i <= k) and ans do begin
                j2 := 1;
                j := 1;
                while (j <= n) and ans do begin
                    if a[1][j] <> a[i][0] then begin
                        ans := a[1][j] = a[i][j2];
                        inc(j2);
                    end;
                    inc(j);
                end;
                inc(i);
            end;

            a[1][j1] := a[1][j1-1];
            dec(j1);

        end;

        if ans then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
