program A_ab;
const
    maxn = 100;
var
    n, i: int8;
    ans: boolean;
    s: array [1 .. maxn] of char;

begin
    readln(n);

    ans := false;
    read(s[1]);
    for i := 2 to n do begin
        read(s[i]);
        ans := ans or ((s[i-1] = 'a') and (s[i] = 'b') or (s[i-1] = 'b') and (s[i] = 'a'));
    end;
    readln;

    if ans then
        writeln('Yes')
    else
        writeln('No');

end.
