# Problem: B_Qingshan_Loves_Strings.pas

```pascal
program B_Qingshan_Loves_Strings;
uses
    math;
const
    maxn = 50;
var
    ntc, tci: int16;
    n, m, i: int8;
    ans: boolean;
    s, t: array [1 .. maxn] of char;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

        for i := 1 to n do read(s[i]); readln;
        for i := 1 to m do read(t[i]); readln;

        i := 1;
        while (i < n) and (s[i] <> s[i+1]) do inc(i);
        ans := i = n;

        if not ans then begin

            i := 1;
            while (i < m) and (t[i] <> t[i+1]) do inc(i);
            ans := (i = m) and odd(m);

            if ans then begin

                i := 1;
                while (i < n) and ((s[i] <> s[i+1]) or (s[i] <> t[1])) do inc(i);
                ans := i = n;

            end;

        end;

        if ans then
            writeln('Yes')
        else
            writeln('No');

    end;
end.

```
