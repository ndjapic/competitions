program E_Stamp;
const
    maxn = 200 * 1000;
var
    n, m, i, j, l, r: int32;
    loop: boolean;
    s, t: array [1 .. maxn] of char;

begin
    readln(n, m);
    for i := 1 to n do read(s[i]); readln;
    for j := 1 to m do read(t[j]); readln;

    l := 1;
    r := n;
    loop := true;
    while (r-l+1 >= m) and loop do begin

        j := 1;
        while (j <= m) and (s[l+j-1] = t[j]) do inc(j);

        if j > 1 then
            inc(l, j-1)
        else begin

            j := m;
            while (j > 0) and (s[r-m+j] = t[j]) do dec(j);

            if j < m then
                dec(r, m-j)
            else
                loop := false;

        end;

    end;

    if l <= r then
        writeln('No')
    else
        writeln('Yes');
end.
