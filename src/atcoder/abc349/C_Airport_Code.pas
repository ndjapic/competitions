program C_Airport_Code;
{$H+}
const
    maxn = 100;
var
    n, i: int32;
    s, t: string;

begin
    readln(s);
    readln(t);
    n := length(s);

    i := 1;
    while (i <= n) and (ord(s[i]) - ord('a') <> ord(t[1]) - ord('A')) do inc(i);

    inc(i);
    while (i <= n) and (ord(s[i]) - ord('a') <> ord(t[2]) - ord('A')) do inc(i);

    if t[3] <> 'X' then begin
        inc(i);
        while (i <= n) and (ord(s[i]) - ord('a') <> ord(t[3]) - ord('A')) do inc(i);
    end;

    if i <= n then
        writeln('Yes')
    else
        writeln('No');
end.
