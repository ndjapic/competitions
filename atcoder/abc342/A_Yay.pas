program A_Yay;
{$H+}
const
    maxn = 100;
var
    i, x: int8;
    s: string;

begin
    readln(s);

    if (s[1] = s[2]) or (s[1] = s[3]) then
        i := 1
    else
        i := 2;

    x := 1;
    while s[x] = s[i] do inc(x);
    writeln(x);
end.
