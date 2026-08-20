program C_Takahashi_Gets_Lost;
{$H+}
const
    maxn = 500;
var
    h, w, n, i0, j0, i, j, k: int16;
    ans: int32;
    t: string;
    s: array [1 .. maxn] of string;

begin
    readln(h, w, n);

    readln(t);
    for i := 1 to h do readln(s[i]);

    ans := 0;
    for i0 := 2 to h-1 do
        for j0 := 2 to w-1 do begin
            i := i0;
            j := j0;
            k := 0;
            while (k < n) and (s[i][j] = '.') do begin
                inc(k);
                case t[k] of
                    'L': dec(j);
                    'R': inc(j);
                    'U': dec(i);
                    'D': inc(i);
                end;
            end;
            if s[i][j] = '.' then inc(ans);
        end;

    writeln(ans);
end.
