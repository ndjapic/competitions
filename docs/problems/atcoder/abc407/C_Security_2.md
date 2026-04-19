# Problem: C_Security_2.pas

```pascal
program C_Security_2;
{$MODE DELPHI}
const
    nn = 500 * 1000;
var
    n, i, d, x, ans: int32;
    s: string;

begin
    readln(s);
    n := length(s);

    x := 0;
    ans := n;
    for i := n downto 1 do begin
        d := ord(s[i]) - ord('0');
        d := x-d+10;
        d := 10 - d mod 10;
        d := d mod 10;
        inc(ans, d);
        x := (x+d) mod 10;
    end;

    writeln(ans);
end.

```
