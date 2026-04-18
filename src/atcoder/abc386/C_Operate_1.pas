program C_Operate_1;
{$mode delphi}
uses
    math;
const
    nn = 500 * 1000;
var
    n, m, i, k, mx, l: int32;
    ans: boolean;
    s, t: string;

begin
    readln(k);
    readln(s);
    readln(t);
    n := length(s);
    m := length(t);
    mx := max(n, m);

    ans := abs(n-m) <= 1;

    if ans then begin

        l := 1;
        while (l <= mx) and (s[l] = t[l]) do inc(l);
        ans := l > mx;

        if not ans then begin

            i := 0;
            while (n-i >= l) and (m-i >= l) and (s[n-i] = t[m-i]) do inc(i);
            ans := (n-i < l) or (m-i < l);

            if not ans then ans := (n-i = l) and (m-i = l);

        end;

    end;

    if ans then
        writeln('Yes')
    else
        writeln('No');
end.
