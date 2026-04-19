# Problem: A_Verify_Password.pas

```pascal
program A_Verify_Password;
{$H+}
var
    ntc, tci: int16;
    n, i: int8;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        i := 2;
        while (i <= n) and (s[i-1] <= s[i]) do inc(i);

        if i <= n then
            writeln('NO')
        else
            writeln('YES');

    end;
end.

```
